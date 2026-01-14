unit Test.BoldExternalizedReferences;

interface

uses
  DUnitX.TestFramework,
  BoldExternalizedReferences;

type
  [TestFixture]
  TTestBoldExternalizedReferences = class
  private
    FList: TBoldExternalizedReferenceList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreate;
    [Test]
    procedure TestCountEmpty;
    [Test]
    procedure TestSetAndGetReferencedObject;
    [Test]
    procedure TestGetReferencedObject_NotFound;
    [Test]
    procedure TestCountAfterAdd;
    [Test]
    procedure TestSetReferencedObject_Replace;
    [Test]
    procedure TestSetReferencedObject_RemoveWithNil;
    [Test]
    procedure TestManageReferencedObjectProperty;
    [Test]
    procedure TestSetReferencedObject_SameValueNoOp;
    [Test]
    procedure TestMultipleReferees;
  end;

implementation

uses
  SysUtils,
  Classes;

{ TTestBoldExternalizedReferences }

procedure TTestBoldExternalizedReferences.Setup;
begin
  FList := TBoldExternalizedReferenceList.Create;
end;

procedure TTestBoldExternalizedReferences.TearDown;
begin
  FList.Free;
end;

procedure TTestBoldExternalizedReferences.TestCreate;
begin
  Assert.IsNotNull(FList);
end;

procedure TTestBoldExternalizedReferences.TestCountEmpty;
begin
  Assert.AreEqual(0, FList.Count);
end;

procedure TTestBoldExternalizedReferences.TestSetAndGetReferencedObject;
var
  Referee, Referenced: TObject;
begin
  Referee := TObject.Create;
  Referenced := TObject.Create;
  try
    FList.ReferencedObjects[Referee] := Referenced;
    Assert.AreSame(Referenced, FList.ReferencedObjects[Referee]);
  finally
    Referee.Free;
    Referenced.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestGetReferencedObject_NotFound;
var
  Referee: TObject;
begin
  Referee := TObject.Create;
  try
    Assert.IsNull(FList.ReferencedObjects[Referee]);
  finally
    Referee.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestCountAfterAdd;
var
  Referee1, Referee2, Referenced1, Referenced2: TObject;
begin
  Referee1 := TObject.Create;
  Referee2 := TObject.Create;
  Referenced1 := TObject.Create;
  Referenced2 := TObject.Create;
  try
    FList.ReferencedObjects[Referee1] := Referenced1;
    Assert.AreEqual(1, FList.Count);

    FList.ReferencedObjects[Referee2] := Referenced2;
    Assert.AreEqual(2, FList.Count);
  finally
    Referee1.Free;
    Referee2.Free;
    Referenced1.Free;
    Referenced2.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestSetReferencedObject_Replace;
var
  Referee, Referenced1, Referenced2: TObject;
begin
  Referee := TObject.Create;
  Referenced1 := TObject.Create;
  Referenced2 := TObject.Create;
  try
    FList.ReferencedObjects[Referee] := Referenced1;
    Assert.AreSame(Referenced1, FList.ReferencedObjects[Referee]);
    Assert.AreEqual(1, FList.Count);

    FList.ReferencedObjects[Referee] := Referenced2;
    Assert.AreSame(Referenced2, FList.ReferencedObjects[Referee]);
    Assert.AreEqual(1, FList.Count);  // Count should still be 1 after replacement
  finally
    Referee.Free;
    Referenced1.Free;
    Referenced2.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestSetReferencedObject_RemoveWithNil;
var
  Referee, Referenced: TObject;
begin
  Referee := TObject.Create;
  Referenced := TObject.Create;
  try
    FList.ReferencedObjects[Referee] := Referenced;
    Assert.AreEqual(1, FList.Count);

    FList.ReferencedObjects[Referee] := nil;
    Assert.AreEqual(0, FList.Count);
    Assert.IsNull(FList.ReferencedObjects[Referee]);
  finally
    Referee.Free;
    Referenced.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestManageReferencedObjectProperty;
begin
  Assert.IsFalse(FList.ManageReferencedObject);

  FList.ManageReferencedObject := True;
  Assert.IsTrue(FList.ManageReferencedObject);

  FList.ManageReferencedObject := False;
  Assert.IsFalse(FList.ManageReferencedObject);
end;

procedure TTestBoldExternalizedReferences.TestSetReferencedObject_SameValueNoOp;
var
  Referee, Referenced: TObject;
begin
  Referee := TObject.Create;
  Referenced := TObject.Create;
  try
    FList.ReferencedObjects[Referee] := Referenced;
    Assert.AreEqual(1, FList.Count);

    // Setting same value should be a no-op
    FList.ReferencedObjects[Referee] := Referenced;
    Assert.AreEqual(1, FList.Count);
    Assert.AreSame(Referenced, FList.ReferencedObjects[Referee]);
  finally
    Referee.Free;
    Referenced.Free;
  end;
end;

procedure TTestBoldExternalizedReferences.TestMultipleReferees;
var
  Referee1, Referee2, Referee3: TObject;
  Referenced1, Referenced2, Referenced3: TObject;
begin
  Referee1 := TObject.Create;
  Referee2 := TObject.Create;
  Referee3 := TObject.Create;
  Referenced1 := TObject.Create;
  Referenced2 := TObject.Create;
  Referenced3 := TObject.Create;
  try
    FList.ReferencedObjects[Referee1] := Referenced1;
    FList.ReferencedObjects[Referee2] := Referenced2;
    FList.ReferencedObjects[Referee3] := Referenced3;

    Assert.AreEqual(3, FList.Count);
    Assert.AreSame(Referenced1, FList.ReferencedObjects[Referee1]);
    Assert.AreSame(Referenced2, FList.ReferencedObjects[Referee2]);
    Assert.AreSame(Referenced3, FList.ReferencedObjects[Referee3]);
  finally
    Referee1.Free;
    Referee2.Free;
    Referee3.Free;
    Referenced1.Free;
    Referenced2.Free;
    Referenced3.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldExternalizedReferences);

end.
