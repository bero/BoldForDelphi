unit Test.BoldUMLTaggedValues;

interface

uses
  DUnitX.TestFramework,
  BoldUMLTaggedValues;

type
  [TestFixture]
  TTestBoldUMLTaggedValues = class
  public
    [Test]
    procedure TestUMLTaggedValueListReturnsInstance;
    [Test]
    procedure TestUMLTaggedValueListReturnsSameInstance;
    [Test]
    procedure TestClassTaggedValues;
    [Test]
    procedure TestAssociationTaggedValues;
    [Test]
    procedure TestAttributeTaggedValues;
    [Test]
    procedure TestConstants;
  end;

implementation

uses
  BoldTaggedValueList;

{ TTestBoldUMLTaggedValues }

procedure TTestBoldUMLTaggedValues.TestUMLTaggedValueListReturnsInstance;
begin
  Assert.IsNotNull(UMLTaggedValueList);
end;

procedure TTestBoldUMLTaggedValues.TestUMLTaggedValueListReturnsSameInstance;
var
  List1, List2: TBoldTaggedValuePerClassList;
begin
  List1 := UMLTaggedValueList;
  List2 := UMLTaggedValueList;
  Assert.AreSame(List1, List2);
end;

procedure TTestBoldUMLTaggedValues.TestClassTaggedValues;
var
  List: TBoldTaggedValuePerClassList;
  ClassList: TBoldTaggedValueList;
begin
  List := UMLTaggedValueList;
  ClassList := List.ListForClassName['Class'];
  Assert.IsNotNull(ClassList);
  Assert.IsTrue(ClassList.Count > 0);
end;

procedure TTestBoldUMLTaggedValues.TestAssociationTaggedValues;
var
  List: TBoldTaggedValuePerClassList;
  AssocList: TBoldTaggedValueList;
begin
  List := UMLTaggedValueList;
  AssocList := List.ListForClassName['Association'];
  Assert.IsNotNull(AssocList);
  Assert.IsTrue(AssocList.Count > 0);
end;

procedure TTestBoldUMLTaggedValues.TestAttributeTaggedValues;
var
  List: TBoldTaggedValuePerClassList;
  AttrList: TBoldTaggedValueList;
begin
  List := UMLTaggedValueList;
  AttrList := List.ListForClassName['Attribute'];
  Assert.IsNotNull(AttrList);
  Assert.IsTrue(AttrList.Count > 0);
end;

procedure TTestBoldUMLTaggedValues.TestConstants;
begin
  // Verify string constants are defined correctly
  Assert.AreEqual('documentation', TAG_DOCUMENTATION);
  Assert.AreEqual('derived', TAG_DERIVED);
  Assert.AreEqual('PersistenceEnum', ENUM_TAG_PERSISTENCE);
  Assert.AreEqual('persistence', TAG_PERSISTENCE);
  Assert.AreEqual('persistent', TV_PERSISTENCE_PERSISTENT);
  Assert.AreEqual('transient', TV_PERSISTENCE_TRANSIENT);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldUMLTaggedValues);

end.
