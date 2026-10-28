unit Test.BoldElements;

interface

uses
  BoldTestFramework,
  BoldElements,
  BoldDefs,
  BoldBase,
  BoldSubscription;

type
  /// <summary>
  /// Test fixture for TBoldIndirectElement
  /// Tests reference and owned value management
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldIndirectElement = class(TBoldTestCase)
  private
    fRaiseIndirect: TObject;
    fRaiseElement: TObject;
    procedure TestSetReferenceValueRaisesWhenSameOwnedValue_Raiser;
  published
    procedure TestCreateInitialState;
    procedure TestSetReferenceValue;
    procedure TestSetReferenceValueToNil;
    procedure TestSetOwnedValue;
    procedure TestSetOwnedValueToNil;
    procedure TestTransferValueReference;
    procedure TestTransferValueOwned;
    procedure TestTransferValueToNil;
    procedure TestRelinquishValue;
    procedure TestRelinquishValueClearsOwnership;
    procedure TestSetReferenceValueReplacesOwned;
    procedure TestSetOwnedValueReplacesOwned;
    procedure TestSetSameReferenceValueTwice;
    procedure TestSetSameOwnedValueTwice;
    procedure TestContextObject;
    procedure TestSetReferenceValueRaisesWhenSameOwnedValue;
  end;

  /// <summary>
  /// Test fixture for TBoldMetaElement
  /// Tests metadata elements with name properties
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldMetaElement = class(TBoldTestCase)
  private
    fRaiseMeta: TObject;
    fRaiseResultList: TObject;
    procedure TestDefaultSubscribeOnMutableRaises_Raiser;
    procedure TestGetAsListRaises_Raiser;
  published
    procedure TestCreate;
    procedure TestProperties;
    procedure TestGetStringRepresentation;
    procedure TestDisplayName;
    procedure TestIsImmutableAfterCreate;
    procedure TestIsEqualAs_SameObject;
    procedure TestIsEqualAs_DifferentObject;
    procedure TestDefaultSubscribeOnMutableRaises;
    procedure TestGetAsListRaises;
  end;

  /// <summary>
  /// Test fixture for TBoldExternalVariableList
  /// Tests variable list operations
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldExternalVariableList = class(TBoldTestCase)
  published
    procedure TestCreateDefault;
    procedure TestCreateWithOwnership;
    procedure TestCreateWithoutOwnership;
    procedure TestGetAsCommaTextEmpty;
    procedure TestGetEnumerator;
    procedure TestAddVariable;
    procedure TestGetVariables;
    procedure TestGetVariableByName;
    procedure TestGetVariableByNameNotFound;
    procedure TestGetAsCommaTextWithVariables;
    procedure TestEnumeratorWithVariables;
  end;

  /// <summary>
  /// Test fixture for TBoldElement base functionality
  /// Tests common element operations that can be tested without a full system
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldElementBase = class(TBoldTestCase)
  private
    FElement: TBoldIndirectElement;
    fRaiseElement: TObject;
    fRaiseObserver2: TObject;
    fRaiseSource: TObject;
    fRaiseElement1: TObject;
    fRaiseElement2: TObject;
    fRaiseOtherComp: TObject;
    procedure TestRegisterModifiedValueHolderConflict_Raiser;
    procedure TestAssignRaisesForSource_Raiser;
    procedure TestAssignRaisesForNil_Raiser;
    procedure TestCompareToAsRaises_Raiser;
    procedure TestCompareErrorWithElement_Raiser;
    procedure TestCompareErrorWithNil_Raiser;
    procedure TestMutableErrorRaises_Raiser;
    procedure TestEnsureValidStringRaises_Raiser;
    procedure TestGetAsVariant_Raiser;
    procedure TestSetAsVariant_Raiser;
    procedure TestCompareToWithDifferentElement_Raiser;
    procedure TestAssignErrorWithElement_Raiser;
    procedure TestAssignErrorWithNil_Raiser;
    procedure TestCompareTypeError_Raiser;
    procedure TestCompareTypeErrorWithNil_Raiser;
    procedure TestGetStringRepresentationRaises_Raiser;
    procedure TestSetStringRepresentationRaises_Raiser;
    procedure TestRegisterModifiedValueHolderWithComponent_Raiser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestMutableDefault;
    procedure TestMakeImmutable;
    procedure TestValidateCharacterDefault;
    procedure TestValidateStringDefault;
    procedure TestValidateStringEmpty;
    procedure TestIsEqualSameObject;
    procedure TestCompareToSameObject;
    procedure TestGetAsValueSetsReference;
    procedure TestObserverMayModifyNoHolder;
    procedure TestObserverMayModifyImmutable;
    procedure TestRegisterAndUnregisterModifiedValueHolder;
    procedure TestRegisterModifiedValueHolderConflict;
    procedure TestObserverMayModifyAsString;
    procedure TestValidateVariant;
    procedure TestValidateStringWithInvalidChar;
    procedure TestAssignRaisesForSource;
    procedure TestAssignRaisesForNil;
    procedure TestCompareToAsRaises;
    procedure TestCompareErrorWithElement;
    procedure TestCompareErrorWithNil;
    procedure TestMutableErrorRaises;
    procedure TestEnsureValidStringRaises;
    procedure TestGetAsVariant;
    procedure TestSetAsVariant;
    procedure TestGetDisplayNameWithBoldType;
    procedure TestGetDisplayNameWithoutBoldType;
    procedure TestGetIsPartOfSystemDefault;
    procedure TestGetContextString;
    procedure TestCloneIfPossible;
    procedure TestCompareToWithDifferentElement;
    procedure TestSubscribeToStringRepresentation;
    procedure TestAssignErrorWithElement;
    procedure TestAssignErrorWithNil;
    procedure TestCompareTypeError;
    procedure TestCompareTypeErrorWithNil;
    procedure TestGetStringRepresentationRaises;
    procedure TestSetStringRepresentationRaises;
    procedure TestRegisterModifiedValueHolderWithComponent;
    procedure TestRegisterSameHolderTwice;
  end;

  /// <summary>
  /// Test fixture for TBoldListTypeInfo
  /// Tests list type information and conformance
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldListTypeInfo = class(TBoldTestCase)
  published
    procedure TestCreateWithNilElementType;
    procedure TestGetStringRepresentationNilElement;
    procedure TestConformsToWithNilElementType;
    procedure TestConformsToNonListType;
    procedure TestGetListTypeInfoReturnsSelf;
    procedure TestCreateWithElementType;
    procedure TestGetStringRepresentationWithElement;
    procedure TestConformsToWithMatchingElementType;
  end;

  /// <summary>
  /// Test fixture for TBoldElementTypeInfo
  /// Tests element type information
  /// </summary>
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldElementTypeInfo = class(TBoldTestCase)
  private
    fRaiseTypeInfo: TObject;
    procedure TestCreateElementRaises_Raiser;
    procedure TestGetListTypeInfoRaises_Raiser;
  published
    procedure TestElementClassReturnsNil;
    procedure TestCreateElementRaises;
    procedure TestGetListTypeInfoRaises;
  end;

  /// <summary>
  /// Concrete test element for testing TBoldElement abstract methods
  /// </summary>
  TTestBoldElement = class(TBoldElement)
  private
    FBoldType: TBoldElementTypeInfo;
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetIsPartOfSystem: Boolean; override;
  public
    constructor Create;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
  end;

  /// <summary>
  /// Concrete meta element that resolves the abstract GetBoldType
  /// </summary>
  TTestConcreteMetaElement = class(TBoldMetaElement)
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
  end;

  /// <summary>
  /// Mutable meta element for testing DefaultSubscribe exception
  /// </summary>
  TTestMutableMetaElement = class(TTestConcreteMetaElement)
  public
    constructor Create;
  end;

  /// <summary>
  /// Cracker class for accessing protected members of TBoldIndirectElement
  /// </summary>
  TBoldIndirectElementCracker = class(TBoldIndirectElement);

  /// <summary>
  /// Cracker class for accessing protected members of TBoldElement
  /// </summary>
  TBoldElementCracker = class(TBoldElement);

  /// <summary>
  /// Test element that rejects digits for validation testing
  /// </summary>
  TTestValidatingElement = class(TBoldElement)
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetIsPartOfSystem: Boolean; override;
  public
    function ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean; override;
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
  end;

  /// <summary>
  /// Test element with a mock BoldType for testing DisplayName
  /// </summary>
  TTestElementWithType = class(TBoldElement)
  private
    FBoldType: TBoldElementTypeInfo;
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
    function GetIsPartOfSystem: Boolean; override;
  public
    constructor Create(ABoldType: TBoldElementTypeInfo);
    procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override;
    procedure GetAsList(ResultList: TBoldIndirectElement); override;
  end;

  /// <summary>
  /// Concrete TBoldElementTypeInfo for testing base class methods
  /// </summary>
  TTestElementTypeInfo = class(TBoldElementTypeInfo)
  protected
    function GetBoldType: TBoldElementTypeInfo; override;
  public
    function ConformsTo(Element: TBoldElementTypeInfo): Boolean; override;
  end;

  /// <summary>
  /// Concrete TBoldExternalVariable for testing variable list
  /// </summary>
  TTestExternalVariable = class(TBoldExternalVariable)
  private
    FValue: TBoldElement;
    FValueType: TBoldElementTypeInfo;
  protected
    function GetValue: TBoldElement; override;
    function GetValueType: TBoldElementTypeInfo; override;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TTestBoldElement }

constructor TTestBoldElement.Create;
begin
  inherited Create;
  FBoldType := nil;
end;

function TTestBoldElement.GetBoldType: TBoldElementTypeInfo;
begin
  Result := FBoldType;
end;

function TTestBoldElement.GetIsPartOfSystem: Boolean;
begin
  Result := False; // Allow TBoldIndirectElement to free owned instances
end;

procedure TTestBoldElement.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  // No-op for testing
end;

procedure TTestBoldElement.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

{ TTestConcreteMetaElement }

function TTestConcreteMetaElement.GetBoldType: TBoldElementTypeInfo;
begin
  Result := nil;
end;

{ TTestMutableMetaElement }

constructor TTestMutableMetaElement.Create;
begin
  inherited Create('Model', 'Expr', 'Delphi');
  // Base constructor calls MakeImmutable, so we need to make it mutable again for testing
  // Since SetElementFlag is private, we use PrepareToDestroy which resets immutable flag
  PrepareToDestroy;
end;

{ TTestBoldIndirectElement }

procedure TTestBoldIndirectElement.TestSetReferenceValueRaisesWhenSameOwnedValue_Raiser;
begin
  TBoldIndirectElement(fRaiseIndirect).SetReferenceValue(TTestBoldElement(fRaiseElement));
end;

procedure TTestBoldIndirectElement.TestCreateInitialState;
var
  Indirect: TBoldIndirectElement;
begin
  Indirect := TBoldIndirectElement.Create;
  try
    Assert.IsNull(Indirect.Value, 'Value should be nil initially');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own value initially');
  finally
    Indirect.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetReferenceValue;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetReferenceValue(Element);
    Assert.AreSame(Element, Indirect.Value, 'Value should be set');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own reference value');
  finally
    Indirect.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetReferenceValueToNil;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetReferenceValue(Element);
    Indirect.SetReferenceValue(nil);
    Assert.IsNull(Indirect.Value, 'Value should be nil after setting to nil');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own nil value');
  finally
    Indirect.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetOwnedValue;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    Assert.AreSame(Element, Indirect.Value, 'Value should be set');
    Assert.IsTrue(Indirect.OwnsValue, 'Should own the value');
  finally
    Indirect.Free;
    // Element is owned by Indirect, will be freed
  end;
end;

procedure TTestBoldIndirectElement.TestSetOwnedValueToNil;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    Indirect.SetOwnedValue(nil);
    Assert.IsNull(Indirect.Value, 'Value should be nil');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own nil value');
  finally
    Indirect.Free;
    // Element was freed when replaced
  end;
end;

procedure TTestBoldIndirectElement.TestTransferValueReference;
var
  Source, Target: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Source := TBoldIndirectElement.Create;
  Target := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Source.SetReferenceValue(Element);
    Source.TransferValue(Target);

    Assert.IsNull(Source.Value, 'Source should be cleared');
    Assert.IsFalse(Source.OwnsValue, 'Source should not own');
    Assert.AreSame(Element, Target.Value, 'Target should have value');
    Assert.IsFalse(Target.OwnsValue, 'Target should not own reference');
  finally
    Source.Free;
    Target.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestTransferValueOwned;
var
  Source, Target: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Source := TBoldIndirectElement.Create;
  Target := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Source.SetOwnedValue(Element);
    Source.TransferValue(Target);

    Assert.IsNull(Source.Value, 'Source should be cleared');
    Assert.IsFalse(Source.OwnsValue, 'Source should not own');
    Assert.AreSame(Element, Target.Value, 'Target should have value');
    Assert.IsTrue(Target.OwnsValue, 'Target should own transferred value');
  finally
    Source.Free;
    Target.Free;
    // Element owned by Target
  end;
end;

procedure TTestBoldIndirectElement.TestTransferValueToNil;
var
  Source: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Source := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Source.SetReferenceValue(Element);
    Source.TransferValue(nil);

    Assert.IsNull(Source.Value, 'Source should be cleared');
    Assert.IsFalse(Source.OwnsValue, 'Source should not own');
  finally
    Source.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestRelinquishValue;
var
  Indirect: TBoldIndirectElement;
  Element, Relinquished: TTestBoldElement;
begin
  Relinquished := nil;
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    Relinquished := TTestBoldElement(Indirect.RelinquishValue);

    Assert.AreSame(Element, Relinquished, 'Should return the owned value');
    Assert.IsNull(Indirect.Value, 'Value should be nil after relinquish');
  finally
    Indirect.Free;
    Relinquished.Free; // Now we own it
  end;
end;

procedure TTestBoldIndirectElement.TestRelinquishValueClearsOwnership;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    Indirect.RelinquishValue;

    Assert.IsFalse(Indirect.OwnsValue, 'Should not own after relinquish');
  finally
    Indirect.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetReferenceValueReplacesOwned;
var
  Indirect: TBoldIndirectElement;
  OwnedElement, RefElement: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  OwnedElement := TTestBoldElement.Create;
  RefElement := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(OwnedElement);
    // OwnedElement is now owned by Indirect
    Indirect.SetReferenceValue(RefElement);
    // OwnedElement should be freed, RefElement set

    Assert.AreSame(RefElement, Indirect.Value, 'Should have new reference value');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own reference value');
  finally
    Indirect.Free;
    RefElement.Free;
    // OwnedElement was freed when replaced
  end;
end;

procedure TTestBoldIndirectElement.TestSetOwnedValueReplacesOwned;
var
  Indirect: TBoldIndirectElement;
  Element1, Element2: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element1);
    Indirect.SetOwnedValue(Element2);

    Assert.AreSame(Element2, Indirect.Value, 'Should have new owned value');
    Assert.IsTrue(Indirect.OwnsValue, 'Should own new value');
  finally
    Indirect.Free;
    // Both elements were freed when replaced or on destroy
  end;
end;

procedure TTestBoldIndirectElement.TestSetSameReferenceValueTwice;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetReferenceValue(Element);
    Indirect.SetReferenceValue(Element); // Same value again

    Assert.AreSame(Element, Indirect.Value, 'Should still have value');
    Assert.IsFalse(Indirect.OwnsValue, 'Should not own reference');
  finally
    Indirect.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetSameOwnedValueTwice;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    // Setting the same owned value should work without raising
    Indirect.SetOwnedValue(Element);

    Assert.AreSame(Element, Indirect.Value, 'Should still have value');
    Assert.IsTrue(Indirect.OwnsValue, 'Should own value');
  finally
    Indirect.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestContextObject;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Assert.IsNull(TBoldIndirectElementCracker(Indirect).ContextObject, 'ContextObject should be nil initially');
    Indirect.SetReferenceValue(Element);
    Assert.IsTrue(TBoldIndirectElementCracker(Indirect).ContextObject = Element, 'ContextObject should return Value');
  finally
    Indirect.Free;
    Element.Free;
  end;
end;

procedure TTestBoldIndirectElement.TestSetReferenceValueRaisesWhenSameOwnedValue;
var
  Indirect: TBoldIndirectElement;
  Element: TTestBoldElement;
begin
  Indirect := TBoldIndirectElement.Create;
  Element := TTestBoldElement.Create;
  try
    Indirect.SetOwnedValue(Element);
    // Setting the same value as reference when it's owned should raise
    fRaiseIndirect := Indirect;
    fRaiseElement := Element;
    Assert.WillRaise(TestSetReferenceValueRaisesWhenSameOwnedValue_Raiser, EBold);
  finally
    Indirect.Free;
    // Element was freed by the raised exception handler or will be freed by Indirect
  end;
end;

{ TTestBoldMetaElement }

procedure TTestBoldMetaElement.TestDefaultSubscribeOnMutableRaises_Raiser;
begin
  TTestMutableMetaElement(fRaiseMeta).DefaultSubscribe(nil);
end;

procedure TTestBoldMetaElement.TestGetAsListRaises_Raiser;
begin
  TTestConcreteMetaElement(fRaiseMeta).GetAsList(TBoldIndirectElement(fRaiseResultList));
end;

procedure TTestBoldMetaElement.TestCreate;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('ModelName', 'ExprName', 'DelphiName');
  try
    Assert.IsNotNull(Meta, 'Should create successfully');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestProperties;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('TestModel', 'TestExpr', 'TestDelphi');
  try
    Assert.AreEqual('TestModel', Meta.ModelName, 'ModelName mismatch');
    Assert.AreEqual('TestExpr', Meta.ExpressionName, 'ExpressionName mismatch');
    Assert.AreEqual('TestDelphi', Meta.DelphiName, 'DelphiName mismatch');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestGetStringRepresentation;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('Model', 'Expression', 'Delphi');
  try
    Assert.AreEqual('Expression', Meta.AsString, 'AsString should return ExpressionName');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestDisplayName;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('Model', 'ExpressionName', 'Delphi');
  try
    Assert.AreEqual('ExpressionName', Meta.DisplayName, 'DisplayName should return ExpressionName');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestIsImmutableAfterCreate;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  try
    Assert.IsFalse(Meta.Mutable, 'Should be immutable after creation');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestIsEqualAs_SameObject;
var
  Meta: TTestConcreteMetaElement;
begin
  Meta := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  try
    Assert.IsTrue(Meta.IsEqualAs(ctDefault, Meta), 'Should be equal to itself');
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestIsEqualAs_DifferentObject;
var
  Meta1, Meta2: TTestConcreteMetaElement;
begin
  Meta1 := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  Meta2 := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  try
    Assert.IsFalse(Meta1.IsEqualAs(ctDefault, Meta2), 'Different objects should not be equal');
  finally
    Meta1.Free;
    Meta2.Free;
  end;
end;

procedure TTestBoldMetaElement.TestDefaultSubscribeOnMutableRaises;
var
  Meta: TTestMutableMetaElement;
begin
  Meta := TTestMutableMetaElement.Create;
  try
    fRaiseMeta := Meta;
    Assert.WillRaise(TestDefaultSubscribeOnMutableRaises_Raiser, EBold);
  finally
    Meta.Free;
  end;
end;

procedure TTestBoldMetaElement.TestGetAsListRaises;
var
  Meta: TTestConcreteMetaElement;
  ResultList: TBoldIndirectElement;
begin
  Meta := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  ResultList := TBoldIndirectElement.Create;
  try
    fRaiseMeta := Meta;
    fRaiseResultList := ResultList;
    Assert.WillRaise(TestGetAsListRaises_Raiser, EBoldFeatureNotImplementedYet);
  finally
    Meta.Free;
    ResultList.Free;
  end;
end;

{ TTestBoldExternalVariableList }

procedure TTestBoldExternalVariableList.TestCreateDefault;
var
  List: TBoldExternalVariableList;
begin
  List := TBoldExternalVariableList.Create;
  try
    Assert.AreEqual(0, List.Count, 'Should be empty');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestCreateWithOwnership;
var
  List: TBoldExternalVariableList;
begin
  List := TBoldExternalVariableList.Create(True);
  try
    Assert.AreEqual(0, List.Count, 'Should be empty');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestCreateWithoutOwnership;
var
  List: TBoldExternalVariableList;
begin
  List := TBoldExternalVariableList.Create(False);
  try
    Assert.AreEqual(0, List.Count, 'Should be empty');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestGetAsCommaTextEmpty;
var
  List: TBoldExternalVariableList;
begin
  List := TBoldExternalVariableList.Create;
  try
    Assert.AreEqual('', List.AsCommaText, 'Empty list should have empty comma text');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestGetEnumerator;
var
  List: TBoldExternalVariableList;
  Enumerator: TBoldExternalVariableListTraverser;
  Count: Integer;
begin
  List := TBoldExternalVariableList.Create;
  try
    Enumerator := List.GetEnumerator;
    try
      Count := 0;
      while Enumerator.MoveNext do
        Inc(Count);
      Assert.AreEqual(0, Count, 'Empty list should enumerate zero items');
    finally
      Enumerator.Free;
    end;
  finally
    List.Free;
  end;
end;

{ TTestBoldElementBase }

procedure TTestBoldElementBase.TestRegisterModifiedValueHolderConflict_Raiser;
begin
  TTestBoldElement(fRaiseElement).RegisterModifiedValueHolder(TObject(fRaiseObserver2));
end;

procedure TTestBoldElementBase.TestAssignRaisesForSource_Raiser;
begin
  TTestBoldElement(fRaiseElement).Assign(TTestBoldElement(fRaiseSource));
end;

procedure TTestBoldElementBase.TestAssignRaisesForNil_Raiser;
begin
  TTestBoldElement(fRaiseElement).Assign(nil);
end;

procedure TTestBoldElementBase.TestCompareToAsRaises_Raiser;
begin
  TTestBoldElement(fRaiseElement1).CompareToAs(ctDefault, TTestBoldElement(fRaiseElement2));
end;

procedure TTestBoldElementBase.TestCompareErrorWithElement_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement1)).CompareError(TTestBoldElement(fRaiseElement2));
end;

procedure TTestBoldElementBase.TestCompareErrorWithNil_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement)).CompareError(nil);
end;

procedure TTestBoldElementBase.TestMutableErrorRaises_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement)).MutableError('NewValue');
end;

procedure TTestBoldElementBase.TestEnsureValidStringRaises_Raiser;
begin
  TTestValidatingElement(fRaiseElement).EnsureValidString('Hello123', brDefault);
end;

procedure TTestBoldElementBase.TestGetAsVariant_Raiser;
var
  V: Variant;
begin
  V := TTestBoldElement(fRaiseElement).AsVariant;
end;

procedure TTestBoldElementBase.TestSetAsVariant_Raiser;
begin
  TTestBoldElement(fRaiseElement).AsVariant := 'Test';
end;

procedure TTestBoldElementBase.TestCompareToWithDifferentElement_Raiser;
begin
  TTestBoldElement(fRaiseElement1).CompareTo(TTestBoldElement(fRaiseElement2));
end;

procedure TTestBoldElementBase.TestAssignErrorWithElement_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement1)).AssignError(TTestBoldElement(fRaiseElement2));
end;

procedure TTestBoldElementBase.TestAssignErrorWithNil_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement)).AssignError(nil);
end;

procedure TTestBoldElementBase.TestCompareTypeError_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement1)).CompareTypeError(ctDefault, TTestBoldElement(fRaiseElement2));
end;

procedure TTestBoldElementBase.TestCompareTypeErrorWithNil_Raiser;
begin
  TBoldElementCracker(TTestBoldElement(fRaiseElement)).CompareTypeError(ctDefault, nil);
end;

procedure TTestBoldElementBase.TestGetStringRepresentationRaises_Raiser;
var
  S: string;
begin
  S := TTestBoldElement(fRaiseElement).StringRepresentation[brDefault];
end;

procedure TTestBoldElementBase.TestSetStringRepresentationRaises_Raiser;
begin
  TTestBoldElement(fRaiseElement).StringRepresentation[brDefault] := 'Test';
end;

procedure TTestBoldElementBase.TestRegisterModifiedValueHolderWithComponent_Raiser;
begin
  TTestBoldElement(fRaiseElement).RegisterModifiedValueHolder(TComponent(fRaiseOtherComp));
end;

procedure TTestBoldElementBase.SetUp;
begin
  FElement := TBoldIndirectElement.Create;
end;

procedure TTestBoldElementBase.TearDown;
begin
  FElement.Free;
end;

procedure TTestBoldElementBase.TestMutableDefault;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.Mutable, 'Should be mutable by default');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestMakeImmutable;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.Mutable, 'Should be mutable initially');
    Element.MakeImmutable;
    Assert.IsFalse(Element.Mutable, 'Should be immutable after MakeImmutable');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestValidateCharacterDefault;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.ValidateCharacter('A', brDefault), 'Should validate any character');
    Assert.IsTrue(Element.ValidateCharacter('!', brDefault), 'Should validate special character');
    Assert.IsTrue(Element.ValidateCharacter(' ', brDefault), 'Should validate space');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestValidateStringDefault;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.ValidateString('Hello World!', brDefault), 'Should validate any string');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestValidateStringEmpty;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.ValidateString('', brDefault), 'Should validate empty string');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestIsEqualSameObject;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.IsEqual(Element), 'Element should be equal to itself');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareToSameObject;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.AreEqual(0, Element.CompareTo(Element), 'CompareTo self should return 0');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetAsValueSetsReference;
var
  Element: TTestBoldElement;
  Result: TBoldIndirectElement;
begin
  Element := TTestBoldElement.Create;
  Result := TBoldIndirectElement.Create;
  try
    Element.GetAsValue(Result);
    Assert.AreSame(Element, Result.Value, 'GetAsValue should set reference');
    Assert.IsFalse(Result.OwnsValue, 'Should be reference, not owned');
  finally
    Element.Free;
    Result.Free;
  end;
end;

procedure TTestBoldElementBase.TestObserverMayModifyNoHolder;
var
  Element: TTestBoldElement;
  Observer: TObject;
begin
  Element := TTestBoldElement.Create;
  Observer := TObject.Create;
  try
    Assert.IsTrue(Element.ObserverMayModify(Observer), 'Should allow modification when no holder');
  finally
    Element.Free;
    Observer.Free;
  end;
end;

procedure TTestBoldElementBase.TestObserverMayModifyImmutable;
var
  Element: TTestBoldElement;
  Observer: TObject;
begin
  Element := TTestBoldElement.Create;
  Observer := TObject.Create;
  try
    Element.MakeImmutable;
    Assert.IsFalse(Element.ObserverMayModify(Observer), 'Immutable element should not allow modification');
  finally
    Element.Free;
    Observer.Free;
  end;
end;

procedure TTestBoldElementBase.TestRegisterAndUnregisterModifiedValueHolder;
var
  Element: TTestBoldElement;
  Observer: TObject;
begin
  Element := TTestBoldElement.Create;
  Observer := TObject.Create;
  try
    Assert.IsNull(Element.ModifiedValueHolder, 'Should have no holder initially');

    Element.RegisterModifiedValueHolder(Observer);
    Assert.AreSame(Observer, Element.ModifiedValueHolder, 'Holder should be set');

    Element.UnRegisterModifiedValueHolder(Observer);
    Assert.IsNull(Element.ModifiedValueHolder, 'Holder should be cleared');
  finally
    Element.Free;
    Observer.Free;
  end;
end;

{ TTestBoldListTypeInfo }

procedure TTestBoldListTypeInfo.TestCreateWithNilElementType;
var
  ListType: TBoldListTypeInfo;
begin
  ListType := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  try
    Assert.IsNull(ListType.ListElementTypeInfo, 'Element type should be nil');
    Assert.AreEqual('Collection()', ListType.AsString, 'String representation for nil element type');
  finally
    ListType.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestGetStringRepresentationNilElement;
var
  ListType: TBoldListTypeInfo;
begin
  ListType := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  try
    Assert.AreEqual('Collection()', ListType.StringRepresentation[brDefault], 'Should handle nil element type');
  finally
    ListType.Free;
  end;
end;

{ TTestValidatingElement }

function TTestValidatingElement.GetBoldType: TBoldElementTypeInfo;
begin
  Result := nil;
end;

function TTestValidatingElement.GetIsPartOfSystem: Boolean;
begin
  Result := False;
end;

function TTestValidatingElement.ValidateCharacter(C: Char; Representation: TBoldRepresentation): Boolean;
begin
  // Reject digits
  Result := not CharInSet(C, ['0'..'9']);
end;

procedure TTestValidatingElement.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  // No-op
end;

procedure TTestValidatingElement.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

{ TTestElementWithType }

constructor TTestElementWithType.Create(ABoldType: TBoldElementTypeInfo);
begin
  inherited Create;
  FBoldType := ABoldType;
end;

function TTestElementWithType.GetBoldType: TBoldElementTypeInfo;
begin
  Result := FBoldType;
end;

function TTestElementWithType.GetIsPartOfSystem: Boolean;
begin
  Result := False;
end;

procedure TTestElementWithType.DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent);
begin
  // No-op
end;

procedure TTestElementWithType.GetAsList(ResultList: TBoldIndirectElement);
begin
  ResultList.SetReferenceValue(Self);
end;

{ Additional TTestBoldElementBase tests }

procedure TTestBoldElementBase.TestRegisterModifiedValueHolderConflict;
var
  Element: TTestBoldElement;
  Observer1, Observer2: TObject;
begin
  Element := TTestBoldElement.Create;
  Observer1 := TObject.Create;
  Observer2 := TObject.Create;
  try
    Element.RegisterModifiedValueHolder(Observer1);
    fRaiseElement := Element;
    fRaiseObserver2 := Observer2;
    Assert.WillRaise(TestRegisterModifiedValueHolderConflict_Raiser, EBold);
  finally
    Element.UnRegisterModifiedValueHolder(Observer1);
    Element.Free;
    Observer1.Free;
    Observer2.Free;
  end;
end;

procedure TTestBoldElementBase.TestObserverMayModifyAsString;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.ObserverMayModifyAsString(brDefault, nil), 'Should allow modification');
    Element.MakeImmutable;
    Assert.IsFalse(Element.ObserverMayModifyAsString(brDefault, nil), 'Immutable should not allow');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestValidateVariant;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.IsTrue(Element.ValidateVariant('Test', brDefault), 'Should validate string variant');
    Assert.IsTrue(Element.ValidateVariant(123, brDefault), 'Should validate integer variant');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestValidateStringWithInvalidChar;
var
  Element: TTestValidatingElement;
begin
  Element := TTestValidatingElement.Create;
  try
    Assert.IsTrue(Element.ValidateString('Hello', brDefault), 'Should validate letters only');
    Assert.IsFalse(Element.ValidateString('Hello123', brDefault), 'Should reject digits');
    Assert.IsFalse(Element.ValidateString('123', brDefault), 'Should reject all digits');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestAssignRaisesForSource;
var
  Element, Source: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  Source := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    fRaiseSource := Source;
    Assert.WillRaise(TestAssignRaisesForSource_Raiser, EBold);
  finally
    Element.Free;
    Source.Free;
  end;
end;

procedure TTestBoldElementBase.TestAssignRaisesForNil;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestAssignRaisesForNil_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareToAsRaises;
var
  Element1, Element2: TTestBoldElement;
begin
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    fRaiseElement1 := Element1;
    fRaiseElement2 := Element2;
    Assert.WillRaise(TestCompareToAsRaises_Raiser, EBold);
  finally
    Element1.Free;
    Element2.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareErrorWithElement;
var
  Element1, Element2: TTestBoldElement;
begin
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    fRaiseElement1 := Element1;
    fRaiseElement2 := Element2;
    Assert.WillRaise(TestCompareErrorWithElement_Raiser, EBold);
  finally
    Element1.Free;
    Element2.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareErrorWithNil;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestCompareErrorWithNil_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestMutableErrorRaises;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestMutableErrorRaises_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestEnsureValidStringRaises;
var
  Element: TTestValidatingElement;
begin
  Element := TTestValidatingElement.Create;
  try
    // This should not raise
    Element.EnsureValidString('Hello', brDefault);
    // This should raise
    fRaiseElement := Element;
    Assert.WillRaise(TestEnsureValidStringRaises_Raiser, EBoldAssertionFailed);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetAsVariant;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    // GetAsVariant returns AsString which raises for base TBoldElement
    fRaiseElement := Element;
    Assert.WillRaise(TestGetAsVariant_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestSetAsVariant;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    // SetAsVariant sets AsString which raises for base TBoldElement
    fRaiseElement := Element;
    Assert.WillRaise(TestSetAsVariant_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetDisplayNameWithBoldType;
var
  TypeInfo: TTestElementTypeInfo;
  Element: TTestElementWithType;
begin
  TypeInfo := TTestElementTypeInfo.Create('Model', 'TestType', 'Delphi', nil);
  Element := TTestElementWithType.Create(TypeInfo);
  try
    Assert.AreEqual('TestType', Element.DisplayName, 'Should return BoldType.AsString');
  finally
    Element.Free;
    TypeInfo.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetDisplayNameWithoutBoldType;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Assert.AreEqual('TTestBoldElement', Element.DisplayName, 'Should return ClassName when no BoldType');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetIsPartOfSystemDefault;
var
  MetaElement: TBoldMetaElement;
begin
  MetaElement := TTestConcreteMetaElement.Create('Model', 'Expr', 'Delphi');
  try
    // TBoldMetaElement inherits from TBoldElement, so it has GetIsPartOfSystem
    Assert.IsTrue(MetaElement.IsPartOfSystem, 'Default should return true');
  finally
    MetaElement.Free;
  end;
end;

{ TTestElementTypeInfo }

function TTestElementTypeInfo.GetBoldType: TBoldElementTypeInfo;
begin
  Result := nil;
end;

function TTestElementTypeInfo.ConformsTo(Element: TBoldElementTypeInfo): Boolean;
begin
  Result := Element = Self;
end;

{ TTestBoldElementTypeInfo }

procedure TTestBoldElementTypeInfo.TestCreateElementRaises_Raiser;
begin
  TTestElementTypeInfo(fRaiseTypeInfo).CreateElement;
end;

procedure TTestBoldElementTypeInfo.TestGetListTypeInfoRaises_Raiser;
var
  LI: TBoldListTypeInfo;
begin
  LI := TTestElementTypeInfo(fRaiseTypeInfo).ListTypeInfo;
end;

procedure TTestBoldElementTypeInfo.TestElementClassReturnsNil;
var
  TypeInfo: TTestElementTypeInfo;
begin
  TypeInfo := TTestElementTypeInfo.Create('Model', 'Expr', 'Delphi', nil);
  try
    Assert.IsNull(TypeInfo.ElementClass, 'ElementClass should return nil by default');
  finally
    TypeInfo.Free;
  end;
end;

procedure TTestBoldElementTypeInfo.TestCreateElementRaises;
var
  TypeInfo: TTestElementTypeInfo;
begin
  TypeInfo := TTestElementTypeInfo.Create('Model', 'TestType', 'Delphi', nil);
  try
    fRaiseTypeInfo := TypeInfo;
    Assert.WillRaise(TestCreateElementRaises_Raiser, EBold);
  finally
    TypeInfo.Free;
  end;
end;

procedure TTestBoldElementTypeInfo.TestGetListTypeInfoRaises;
var
  TypeInfo: TTestElementTypeInfo;
  LI: TBoldListTypeInfo;
begin
  TypeInfo := TTestElementTypeInfo.Create('Model', 'TestType', 'Delphi', nil);
  LI := nil;
  try
    fRaiseTypeInfo := TypeInfo;
    Assert.WillRaise(TestGetListTypeInfoRaises_Raiser, EBold);
  finally
    TypeInfo.Free;
  end;
end;

{ Additional TTestBoldListTypeInfo tests }

procedure TTestBoldListTypeInfo.TestConformsToWithNilElementType;
var
  ListType1, ListType2: TBoldListTypeInfo;
begin
  // A list type with nil element conforms to another list type with nil element
  ListType1 := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  ListType2 := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  try
    Assert.IsTrue(ListType1.ConformsTo(ListType2), 'Nil element lists should conform');
  finally
    ListType1.Free;
    ListType2.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestConformsToNonListType;
var
  ListType: TBoldListTypeInfo;
  ElementType: TTestElementTypeInfo;
begin
  ListType := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  ElementType := TTestElementTypeInfo.Create('Model', 'Expr', 'Delphi', nil);
  try
    Assert.IsFalse(ListType.ConformsTo(ElementType), 'List should not conform to non-list type');
  finally
    ListType.Free;
    ElementType.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestGetListTypeInfoReturnsSelf;
var
  ListType: TBoldListTypeInfo;
begin
  ListType := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement);
  try
    Assert.AreSame(ListType, ListType.ListTypeInfo, 'ListTypeInfo should return self');
  finally
    ListType.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestCreateWithElementType;
var
  ElementType: TTestElementTypeInfo;
  ListType: TBoldListTypeInfo;
begin
  ElementType := TTestElementTypeInfo.Create('Model', 'TestElement', 'Delphi', nil);
  ListType := TBoldListTypeInfo.Create(ElementType, nil, TBoldIndirectElement);
  try
    Assert.AreSame(ElementType, ListType.ListElementTypeInfo, 'Should store element type');
  finally
    ListType.Free;
    ElementType.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestGetStringRepresentationWithElement;
var
  ElementType: TTestElementTypeInfo;
  ListType: TBoldListTypeInfo;
begin
  ElementType := TTestElementTypeInfo.Create('Model', 'TestElement', 'Delphi', nil);
  ListType := TBoldListTypeInfo.Create(ElementType, nil, TBoldIndirectElement);
  try
    Assert.AreEqual('Collection(TestElement)', ListType.AsString, 'Should include element type name');
  finally
    ListType.Free;
    ElementType.Free;
  end;
end;

procedure TTestBoldListTypeInfo.TestConformsToWithMatchingElementType;
var
  ElementType1, ElementType2: TTestElementTypeInfo;
  ListType1, ListType2: TBoldListTypeInfo;
begin
  ElementType1 := TTestElementTypeInfo.Create('Model', 'TestElement', 'Delphi', nil);
  ElementType2 := TTestElementTypeInfo.Create('Model', 'TestElement', 'Delphi', nil);
  ListType1 := TBoldListTypeInfo.Create(ElementType1, nil, TBoldIndirectElement);
  ListType2 := TBoldListTypeInfo.Create(nil, nil, TBoldIndirectElement); // Nil element accepts any
  try
    // List with element conforms to list with nil element (nil means "any")
    Assert.IsTrue(ListType1.ConformsTo(ListType2), 'List with element should conform to list with nil element');
  finally
    ListType1.Free;
    ListType2.Free;
    ElementType1.Free;
    ElementType2.Free;
  end;
end;

{ TTestExternalVariable }

constructor TTestExternalVariable.Create(const AName: string);
begin
  inherited Create(nil, AName);
  FValue := nil;
  FValueType := nil;
end;

destructor TTestExternalVariable.Destroy;
begin
  inherited;
end;

function TTestExternalVariable.GetValue: TBoldElement;
begin
  Result := FValue;
end;

function TTestExternalVariable.GetValueType: TBoldElementTypeInfo;
begin
  Result := FValueType;
end;

{ Additional TTestBoldExternalVariableList tests }

procedure TTestBoldExternalVariableList.TestAddVariable;
var
  List: TBoldExternalVariableList;
  Variable: TTestExternalVariable;
begin
  List := TBoldExternalVariableList.Create(True); // Owns variables
  Variable := TTestExternalVariable.Create('TestVar');
  try
    List.Add(Variable);
    Assert.AreEqual(1, List.Count, 'Should have one variable');
  finally
    List.Free;
    // Variable is owned by list
  end;
end;

procedure TTestBoldExternalVariableList.TestGetVariables;
var
  List: TBoldExternalVariableList;
  Variable: TTestExternalVariable;
begin
  List := TBoldExternalVariableList.Create(True);
  Variable := TTestExternalVariable.Create('TestVar');
  try
    List.Add(Variable);
    Assert.AreSame(Variable, List.Variables[0], 'Should return the added variable');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestGetVariableByName;
var
  List: TBoldExternalVariableList;
  Var1, Var2: TTestExternalVariable;
  Found: TBoldExternalVariable;
begin
  List := TBoldExternalVariableList.Create(True);
  Var1 := TTestExternalVariable.Create('First');
  Var2 := TTestExternalVariable.Create('Second');
  try
    List.Add(Var1);
    List.Add(Var2);
    Found := List.VariableByName['Second'];
    Assert.AreSame(Var2, Found, 'Should find variable by name');
    // Also test case-insensitive
    Found := List.VariableByName['FIRST'];
    Assert.AreSame(Var1, Found, 'Should find variable case-insensitive');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestGetVariableByNameNotFound;
var
  List: TBoldExternalVariableList;
  Variable: TTestExternalVariable;
  Found: TBoldExternalVariable;
begin
  List := TBoldExternalVariableList.Create(True);
  Variable := TTestExternalVariable.Create('TestVar');
  try
    List.Add(Variable);
    Found := List.VariableByName['NonExistent'];
    Assert.IsNull(Found, 'Should return nil for non-existent variable');
  finally
    List.Free;
  end;
end;

procedure TTestBoldExternalVariableList.TestGetAsCommaTextWithVariables;
var
  List: TBoldExternalVariableList;
  Var1, Var2, Var3: TTestExternalVariable;
begin
  List := TBoldExternalVariableList.Create(True);
  Var1 := TTestExternalVariable.Create('Alpha');
  Var2 := TTestExternalVariable.Create('Beta');
  Var3 := TTestExternalVariable.Create('Gamma');
  try
    List.Add(Var1);
    List.Add(Var2);
    List.Add(Var3);
    Assert.AreEqual('Alpha,Beta,Gamma', List.AsCommaText, 'Should return comma-separated names');
  finally
    List.Free;
  end;
end;

{ Additional TTestBoldElementBase tests }

procedure TTestBoldElementBase.TestGetContextString;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    // GetContextString returns DisplayName, which returns ClassName when BoldType is nil
    Assert.AreEqual('TTestBoldElement', TBoldElementCracker(Element).GetContextString, 'Should return DisplayName');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestCloneIfPossible;
var
  Element: TTestBoldElement;
  Clone: TBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    Clone := TBoldElementCracker(Element).CloneIfPossible;
    Assert.IsNull(Clone, 'Default CloneIfPossible returns nil');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareToWithDifferentElement;
var
  Element1, Element2: TTestBoldElement;
begin
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    // CompareTo with different element calls CompareToAs which raises
    fRaiseElement1 := Element1;
    fRaiseElement2 := Element2;
    Assert.WillRaise(TestCompareToWithDifferentElement_Raiser, EBold);
  finally
    Element1.Free;
    Element2.Free;
  end;
end;

procedure TTestBoldElementBase.TestSubscribeToStringRepresentation;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    // SubscribeToStringRepresentation is a no-op in the base class
    Element.SubscribeToStringRepresentation(brDefault, nil);
    Assert.Pass('Should not raise');
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestAssignErrorWithElement;
var
  Element1, Element2: TTestBoldElement;
begin
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    fRaiseElement1 := Element1;
    fRaiseElement2 := Element2;
    Assert.WillRaise(TestAssignErrorWithElement_Raiser, EBold);
  finally
    Element1.Free;
    Element2.Free;
  end;
end;

procedure TTestBoldElementBase.TestAssignErrorWithNil;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestAssignErrorWithNil_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareTypeError;
var
  Element1, Element2: TTestBoldElement;
begin
  Element1 := TTestBoldElement.Create;
  Element2 := TTestBoldElement.Create;
  try
    fRaiseElement1 := Element1;
    fRaiseElement2 := Element2;
    Assert.WillRaise(TestCompareTypeError_Raiser, EBold);
  finally
    Element1.Free;
    Element2.Free;
  end;
end;

procedure TTestBoldElementBase.TestCompareTypeErrorWithNil;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestCompareTypeErrorWithNil_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestGetStringRepresentationRaises;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestGetStringRepresentationRaises_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestSetStringRepresentationRaises;
var
  Element: TTestBoldElement;
begin
  Element := TTestBoldElement.Create;
  try
    fRaiseElement := Element;
    Assert.WillRaise(TestSetStringRepresentationRaises_Raiser, EBold);
  finally
    Element.Free;
  end;
end;

procedure TTestBoldElementBase.TestRegisterModifiedValueHolderWithComponent;
var
  Element: TTestBoldElement;
  Comp: TComponent;
  OtherComp: TComponent;
begin
  Element := TTestBoldElement.Create;
  Comp := TComponent.Create(nil);
  Comp.Name := 'TestComponent';
  OtherComp := TComponent.Create(nil);
  OtherComp.Name := 'OtherComponent';
  try
    Element.RegisterModifiedValueHolder(Comp);
    Assert.AreSame(Comp, Element.ModifiedValueHolder, 'Should register component');
    // Try to register a different component - should raise
    fRaiseElement := Element;
    fRaiseOtherComp := OtherComp;
    Assert.WillRaise(TestRegisterModifiedValueHolderWithComponent_Raiser, EBold);
  finally
    Element.Free;
    Comp.Free;
    OtherComp.Free;
  end;
end;

procedure TTestBoldElementBase.TestRegisterSameHolderTwice;
var
  Element: TTestBoldElement;
  Observer: TObject;
begin
  Element := TTestBoldElement.Create;
  Observer := TObject.Create;
  try
    Element.RegisterModifiedValueHolder(Observer);
    // Registering the same holder again should not raise
    Element.RegisterModifiedValueHolder(Observer);
    Assert.AreSame(Observer, Element.ModifiedValueHolder, 'Should still have the same holder');
  finally
    Element.Free;
    Observer.Free;
  end;
end;

{ Additional TTestBoldExternalVariableList tests }

procedure TTestBoldExternalVariableList.TestEnumeratorWithVariables;
var
  List: TBoldExternalVariableList;
  Var1, Var2: TTestExternalVariable;
  V: TBoldExternalVariable;
  Count: Integer;
begin
  List := TBoldExternalVariableList.Create(True);
  Var1 := TTestExternalVariable.Create('First');
  Var2 := TTestExternalVariable.Create('Second');
  try
    List.Add(Var1);
    List.Add(Var2);
    Count := 0;
    for V in List do
    begin
      Inc(Count);
      Assert.IsNotNull(V, 'Enumerated variable should not be nil');
    end;
    Assert.AreEqual(2, Count, 'Should enumerate two variables');
  finally
    List.Free;
  end;
end;

initialization
  BoldRegisterTest(TTestBoldIndirectElement);
  BoldRegisterTest(TTestBoldMetaElement);
  BoldRegisterTest(TTestBoldExternalVariableList);
  BoldRegisterTest(TTestBoldElementBase);
  BoldRegisterTest(TTestBoldListTypeInfo);
  BoldRegisterTest(TTestBoldElementTypeInfo);

end.
