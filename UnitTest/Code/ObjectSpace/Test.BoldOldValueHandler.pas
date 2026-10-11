unit Test.BoldOldValueHandler;

{ TBoldOldValueHandler keeps the last saved value of each changed member of a
  saved object. A save of only some of the dirty objects purges the old values
  that equal the current value again, and drops an object's entry once none is
  left. }

interface

uses
  DUnitX.TestFramework,
  BoldSystem,
  BoldTestCaseFireDAC;

type
  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldOldValueHandler = class(TBoldTestCaseFireDAC)
  strict private
    function OldValuesEmpty: Boolean;
    function SavedObject(const aName: string): TBoldObject;
    procedure SaveOnly(aObject: TBoldObject);
  public
    [Test]
    [Category('DB')]
    procedure TestChangedAttributeKeepsAnOldValue;
    [Test]
    [Category('DB')]
    procedure TestPartialSavePurgesAnAttributeChangedBack;
    [Test]
    [Category('DB')]
    procedure TestPartialSaveKeepsAnAttributeStillChanged;
  end;

implementation

type
  // TBoldSystem.OldValueHandler is protected and nothing public exposes it.
  TBoldSystemAccess = class(TBoldSystem);

function TTestBoldOldValueHandler.OldValuesEmpty: Boolean;
begin
  Result := TBoldSystemAccess(System).OldValueHandler.IsEmpty;
end;

function TTestBoldOldValueHandler.SavedObject(const aName: string): TBoldObject;
begin
  Result := CreateObject('TestClass');
  SetAttributeAsString(Result, 'Name', aName);
  UpdateDatabase;
end;

// UpdateDatabaseWithList purges the old values only when other objects stay dirty.
procedure TTestBoldOldValueHandler.SaveOnly(aObject: TBoldObject);
var
  List: TBoldObjectList;
begin
  List := TBoldObjectList.Create;
  try
    List.Add(aObject);
    System.UpdateDatabaseWithList(List);
  finally
    List.Free;
  end;
end;

procedure TTestBoldOldValueHandler.TestChangedAttributeKeepsAnOldValue;
var
  A: TBoldObject;
begin
  A := SavedObject('A');
  Assert.IsTrue(OldValuesEmpty, 'Old values right after the save');

  SetAttributeAsString(A, 'Name', 'A2');
  Assert.IsFalse(OldValuesEmpty, 'Old values after Name changed');
end;

procedure TTestBoldOldValueHandler.TestPartialSavePurgesAnAttributeChangedBack;
var
  A, B: TBoldObject;
begin
  A := SavedObject('A');
  B := SavedObject('B');
  SetAttributeAsString(A, 'Name', 'A2');
  SetAttributeAsString(B, 'Name', 'B2');
  SetAttributeAsString(A, 'Name', 'A');

  SaveOnly(B);
  Assert.IsTrue(A.BoldDirty, 'A stays dirty, so the save purges the old values');
  Assert.IsTrue(OldValuesEmpty, 'Old values after A''s Name was changed back to its saved value');
end;

procedure TTestBoldOldValueHandler.TestPartialSaveKeepsAnAttributeStillChanged;
var
  A, B: TBoldObject;
begin
  A := SavedObject('A');
  B := SavedObject('B');
  SetAttributeAsString(A, 'Name', 'A2');
  SetAttributeAsString(B, 'Name', 'B2');

  SaveOnly(B);
  Assert.IsTrue(A.BoldDirty, 'A stays dirty, so the save purges the old values');
  Assert.IsFalse(OldValuesEmpty, 'Old values while A''s Name still differs from its saved value');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldOldValueHandler);

end.
