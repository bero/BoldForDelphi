unit Test.PersistenceFireDAC;

interface

uses
  DUnitX.TestFramework,
  BoldTestCaseFireDAC;

type
  [TestFixture]
  TTestPersistenceFireDAC = class(TBoldTestCaseFireDAC)
  public
    [Test]
    procedure TestCreateObject;
    [Test]
    procedure TestPersistAndReload;
  end;

implementation

uses
  BoldSystem,
  BoldId;

{ TTestPersistenceFireDAC }

procedure TTestPersistenceFireDAC.TestCreateObject;
var
  Obj: TBoldObject;
begin
  Obj := CreateObject('TestClass');
  Assert.IsNotNull(Obj, 'Object should be created');
  Assert.AreEqual('TestClass', Obj.BoldClassTypeInfo.ExpressionName);
end;

procedure TTestPersistenceFireDAC.TestPersistAndReload;
var
  Obj: TBoldObject;
  ObjId: TBoldObjectId;
  ReloadedObj: TBoldObject;
begin
  // Create object and set attribute
  Obj := CreateObject('TestClass');
  SetAttributeAsString(Obj, 'Name', 'TestValue');

  // Save to database
  UpdateDatabase;

  // Get the ID before refresh
  ObjId := Obj.BoldObjectLocator.BoldObjectID.Clone;
  try
    // Refresh to reload from database
    RefreshSystem;

    // Find the object again and verify
    ReloadedObj := FindObjectById(ObjId);
    Assert.IsNotNull(ReloadedObj, 'Object should be reloaded from database');
    AssertAttributeEquals(ReloadedObj, 'Name', 'TestValue');
  finally
    ObjId.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestPersistenceFireDAC);

end.
