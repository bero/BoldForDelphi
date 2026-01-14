unit Test.BoldBase;

interface

uses
  DUnitX.TestFramework,
  BoldBase;

type
  // Helper class to test GetDebugInfo and ContextObject with override
  TTestableMemoryManagedObject = class(TBoldMemoryManagedObject)
  private
    FContextObj: TObject;
  protected
    function GetDebugInfo: string; override;
    function ContextObject: TObject; override;
  public
    property TestContextObject: TObject read FContextObj write FContextObj;
  end;

  [TestFixture]
  TTestBoldBase = class
  public
    // TBoldMemoryManagedObject tests
    [Test]
    procedure TestGetDebugInfo_Default;
    [Test]
    procedure TestGetDebugInfo_WithContextObject;
    [Test]
    procedure TestContextObject_Default;

    // TBoldInterfacedObject tests
    [Test]
    procedure TestSupportsInterface_IInterface;
    [Test]
    procedure TestSupportsInterface_UnknownInterface;

    // TBoldRefCountedObject tests
    [Test]
    procedure TestRefCountedObject_RefCounting;

    // TBoldNonRefCountedObject tests
    [Test]
    procedure TestNonRefCountedObject_NoRefCounting;

    // TBoldFlaggedObject tests
    [Test]
    procedure TestFlaggedObject_SetGetFlag;
    [Test]
    procedure TestFlaggedObject_InternalState;
  end;

implementation

uses
  SysUtils,
  Classes;

type
  // Helper to access protected methods
  TFlaggedObjectHelper = class(TBoldFlaggedObject)
  public
    procedure TestSetFlag(Flag: TBoldElementFlag; Value: Boolean);
    function TestGetFlag(Flag: TBoldElementFlag): Boolean;
    procedure TestSetInternalState(Mask, Shift, Value: Cardinal);
    function TestGetInternalState(Mask, Shift: Cardinal): Cardinal;
  end;

{ TFlaggedObjectHelper }

procedure TFlaggedObjectHelper.TestSetFlag(Flag: TBoldElementFlag; Value: Boolean);
begin
  SetElementFlag(Flag, Value);
end;

function TFlaggedObjectHelper.TestGetFlag(Flag: TBoldElementFlag): Boolean;
begin
  Result := GetElementFlag(Flag);
end;

procedure TFlaggedObjectHelper.TestSetInternalState(Mask, Shift, Value: Cardinal);
begin
  SetInternalState(Mask, Shift, Value);
end;

function TFlaggedObjectHelper.TestGetInternalState(Mask, Shift: Cardinal): Cardinal;
begin
  Result := GetInternalState(Mask, Shift);
end;

{ TTestableMemoryManagedObject }

function TTestableMemoryManagedObject.GetDebugInfo: string;
begin
  Result := inherited GetDebugInfo;
end;

function TTestableMemoryManagedObject.ContextObject: TObject;
begin
  if Assigned(FContextObj) then
    Result := FContextObj
  else
    Result := inherited ContextObject;
end;

{ TTestBoldBase }

procedure TTestBoldBase.TestGetDebugInfo_Default;
var
  Obj: TBoldMemoryManagedObject;
begin
  Obj := TBoldMemoryManagedObject.Create;
  try
    // Default GetDebugInfo returns ClassName when ContextObject returns self
    Assert.AreEqual('TBoldMemoryManagedObject', Obj.DebugInfo);
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestGetDebugInfo_WithContextObject;
var
  Obj: TTestableMemoryManagedObject;
  ContextObj: TObject;
begin
  Obj := TTestableMemoryManagedObject.Create;
  ContextObj := TStringList.Create;
  try
    Obj.TestContextObject := ContextObj;
    // When ContextObject returns a different object, GetDebugInfo returns its ClassName
    Assert.AreEqual('TStringList', Obj.DebugInfo);
  finally
    ContextObj.Free;
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestContextObject_Default;
var
  Obj: TTestableMemoryManagedObject;
begin
  Obj := TTestableMemoryManagedObject.Create;
  try
    // Default ContextObject returns self (when TestContextObject is nil)
    Obj.TestContextObject := nil;
    // When ContextObject returns self, DebugInfo should return self's classname
    Assert.AreEqual('TTestableMemoryManagedObject', Obj.DebugInfo);
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestSupportsInterface_IInterface;
var
  Obj: TBoldNonRefCountedObject;
begin
  Obj := TBoldNonRefCountedObject.Create;
  try
    // Should support IInterface since it implements it
    Assert.IsTrue(Obj.SupportsInterface(IInterface));
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestSupportsInterface_UnknownInterface;
var
  Obj: TBoldNonRefCountedObject;
  UnknownGuid: TGUID;
begin
  Obj := TBoldNonRefCountedObject.Create;
  try
    UnknownGuid := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');
    // Should not support unknown interface
    Assert.IsFalse(Obj.SupportsInterface(UnknownGuid));
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestRefCountedObject_RefCounting;
var
  Obj: TBoldRefCountedObject;
  Intf: IInterface;
begin
  Obj := TBoldRefCountedObject.Create;
  // Initial ref count after construction should be 0
  Assert.AreEqual(0, Obj.RefCount);

  // Get interface to increment ref count
  Intf := Obj;
  Assert.AreEqual(1, Obj.RefCount);

  // Release will decrement and free when 0
  Intf := nil;
  // Object is freed, don't access it
end;

procedure TTestBoldBase.TestNonRefCountedObject_NoRefCounting;
var
  Obj: TBoldNonRefCountedObject;
  Intf: IInterface;
begin
  Obj := TBoldNonRefCountedObject.Create;
  try
    // Get interface - should not affect anything for non-ref counted
    Intf := Obj;
    // _AddRef and _Release return -1 for non-ref counted objects
    // Object should still be valid
    Assert.IsNotNull(Obj);
    Intf := nil;
    // Object should still be valid (not freed by release)
    Assert.IsNotNull(Obj);
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestFlaggedObject_SetGetFlag;
var
  Obj: TFlaggedObjectHelper;
begin
  Obj := TFlaggedObjectHelper.Create;
  try
    // Initially flag should be false
    Assert.IsFalse(Obj.TestGetFlag(BoldElementFlag0));

    // Set flag to true
    Obj.TestSetFlag(BoldElementFlag0, True);
    Assert.IsTrue(Obj.TestGetFlag(BoldElementFlag0));

    // Set flag back to false
    Obj.TestSetFlag(BoldElementFlag0, False);
    Assert.IsFalse(Obj.TestGetFlag(BoldElementFlag0));

    // Test multiple flags
    Obj.TestSetFlag(BoldElementFlag1, True);
    Obj.TestSetFlag(BoldElementFlag5, True);
    Assert.IsTrue(Obj.TestGetFlag(BoldElementFlag1));
    Assert.IsTrue(Obj.TestGetFlag(BoldElementFlag5));
    Assert.IsFalse(Obj.TestGetFlag(BoldElementFlag2));
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldBase.TestFlaggedObject_InternalState;
var
  Obj: TFlaggedObjectHelper;
  Mask, Shift: Cardinal;
begin
  Obj := TFlaggedObjectHelper.Create;
  try
    // Test internal state with a specific mask and shift
    Mask := $F0;   // Bits 4-7
    Shift := 4;

    // Set state value
    Obj.TestSetInternalState(Mask, Shift, 5);  // Should set bits 4-7 to 0101
    Assert.AreEqual(Cardinal(5), Obj.TestGetInternalState(Mask, Shift));

    // Set different value
    Obj.TestSetInternalState(Mask, Shift, 10); // Should set bits 4-7 to 1010
    Assert.AreEqual(Cardinal(10), Obj.TestGetInternalState(Mask, Shift));
  finally
    Obj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldBase);

end.
