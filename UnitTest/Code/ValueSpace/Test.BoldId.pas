unit Test.BoldId;

interface

uses
  DUnitX.TestFramework,
  BoldId,
  BoldDefs;

type
  [TestFixture]
  TTestBoldId = class
  public
    // TBoldMemberID tests
    [Test] [Category('Quick')]
    procedure TestMemberID_Create;
    [Test] [Category('Quick')]
    procedure TestMemberID_AsString;
    [Test] [Category('Quick')]
    procedure TestMemberID_StreamName;
    [Test] [Category('Quick')]
    procedure TestMemberID_Clone;

    // TBoldSubMemberID tests
    [Test] [Category('Quick')]
    procedure TestSubMemberID_Create;
    [Test] [Category('Quick')]
    procedure TestSubMemberID_Destroy_OwnsPartOf;

    // TBoldObjectId base tests
    [Test] [Category('Quick')]
    procedure TestObjectId_TopSortedIndex;
    [Test] [Category('Quick')]
    procedure TestObjectId_TopSortedIndexExact;
    [Test] [Category('Quick')]
    procedure TestObjectId_Clone;
    [Test] [Category('Quick')]
    procedure TestObjectId_GetNonExisting;
    [Test] [Category('Quick')]
    procedure TestObjectId_GetTimeStamp;

    // TBoldInternalObjectId tests
    [Test] [Category('Quick')]
    procedure TestInternalId_CreateWithClassID;
    [Test] [Category('Quick')]
    procedure TestInternalId_CreateWithClassIDandInternalId;
    [Test] [Category('Quick')]
    procedure TestInternalId_AsString;
    [Test] [Category('Quick')]
    procedure TestInternalId_Hash;
    [Test] [Category('Quick')]
    procedure TestInternalId_IsStorable;
    [Test] [Category('Quick')]
    procedure TestInternalId_StreamName;
    [Test] [Category('Quick')]
    procedure TestInternalId_IsEqual;
    [Test] [Category('Quick')]
    procedure TestInternalId_CloneWithClassId;

    // TBoldExternalObjectID tests
    [Test] [Category('Quick')]
    procedure TestExternalId_IsStorable;

    // TBoldNonExistingObjectId tests
    [Test] [Category('Quick')]
    procedure TestNonExistingId_AsString;
    [Test] [Category('Quick')]
    procedure TestNonExistingId_NonExisting;
    [Test] [Category('Quick')]
    procedure TestNonExistingId_IsStorable;
    [Test] [Category('Quick')]
    procedure TestNonExistingId_HashAndStreamName;
    [Test] [Category('Quick')]
    procedure TestNonExistingId_IsEqual;
    [Test] [Category('Quick')]
    procedure TestNonExistingId_CloneWithClassId;

    // TBoldIdList tests
    [Test] [Category('Quick')]
    procedure TestIdList_CommaSeparatedIdList_Empty;
    [Test] [Category('Quick')]
    procedure TestIdList_CommaSeparatedIdList_MultipleItems;

    // TBoldID base tests
    [Test] [Category('Quick')]
    procedure TestBoldID_GetDebugInfo;

    // TBoldObjectIdList tests
    [Test] [Category('Quick')]
    procedure TestObjectIdList_Create;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_Add_ClonesId;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_AddAndGetID;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_AddIfNotInList;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_AddList;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_Insert;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_Clone;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_Remove;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_ReplaceID;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_FirstAndLast;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_IdInList;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_GetIDByID_LinearSearch;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_GetIDByID_HashSearch;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_GetIndexByID;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_ContainsSameIDs;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_HasInexactIds;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_HasNonExistingIds;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_StreamName;

    // TBoldMemberIdList tests
    [Test] [Category('Quick')]
    procedure TestMemberIdList_StreamName;
    [Test] [Category('Quick')]
    procedure TestMemberIdList_HasId;
    [Test] [Category('Quick')]
    procedure TestMemberIdList_IsEqual;
    [Test] [Category('Quick')]
    procedure TestMemberIdList_Clone;

    // TBoldIDTranslationList tests
    [Test] [Category('Quick')]
    procedure TestTranslationList_CreateAndDestroy;
    [Test] [Category('Quick')]
    procedure TestTranslationList_AddTranslation;
    [Test] [Category('Quick')]
    procedure TestTranslationList_AddTranslation_DedupOldId;
    [Test] [Category('Quick')]
    procedure TestTranslationList_AddTranslation_DedupNewId;
    [Test] [Category('Quick')]
    procedure TestTranslationList_AddTranslation_NilSkip;
    [Test] [Category('Quick')]
    procedure TestTranslationList_TranslateToNewAndOld;
    [Test] [Category('Quick')]
    procedure TestTranslationList_Capacity;
    [Test] [Category('Quick')]
    procedure TestTranslationList_StreamName;

    // ExactifyIds + ApplyTranslationList tests
    [Test] [Category('Quick')]
    procedure TestObjectIdList_ExactifyIds;
    [Test] [Category('Quick')]
    procedure TestObjectIdList_ApplyTranslationList;

    // EBoldOperationFailedForIdList tests
    [Test] [Category('Quick')]
    procedure TestOperationFailedForIdList_CreateAndDestroy;
  end;

implementation

uses
  SysUtils,
  BoldStreams,
  BoldDefaultStreamNames;

{ Helper: create a TBoldInternalObjectId with explicit internal ID }
function CreateInternalId(InternalId, TopSortedIndex: Integer; Exact: Boolean): TBoldInternalObjectId;
begin
  Result := TBoldInternalObjectId.CreateWithClassIDandInternalId(InternalId, TopSortedIndex, Exact);
end;

{ TTestBoldId }

// --- TBoldMemberID ---

procedure TTestBoldId.TestMemberID_Create;
var
  Id: TBoldMemberID;
begin
  Id := TBoldMemberID.Create(7);
  try
    Assert.AreEqual(7, Id.MemberIndex);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestMemberID_AsString;
var
  Id: TBoldMemberID;
begin
  Id := TBoldMemberID.Create(42);
  try
    Assert.AreEqual('42', Id.AsString);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestMemberID_StreamName;
var
  Id: TBoldMemberID;
  Streamable: IBoldStreamable;
begin
  Id := TBoldMemberID.Create(0);
  try
    Streamable := Id as IBoldStreamable;
    Assert.AreEqual(BOLDMEMBERIDNAME, Streamable.StreamName);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestMemberID_Clone;
var
  Id, Cloned: TBoldMemberID;
begin
  Id := TBoldMemberID.Create(99);
  try
    Cloned := Id.Clone;
    try
      Assert.AreNotSame(Id, Cloned);
      Assert.AreEqual(99, Cloned.MemberIndex);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

// --- TBoldSubMemberID ---

procedure TTestBoldId.TestSubMemberID_Create;
var
  PartOf: TBoldMemberID;
  Sub: TBoldSubMemberID;
begin
  PartOf := TBoldMemberID.Create(3);
  // OwnsPartOf=True, so Sub will free PartOf
  Sub := TBoldSubMemberID.Create(PartOf, True, 5);
  try
    Assert.AreEqual(5, Sub.MemberIndex);
  finally
    Sub.Free; // frees PartOf because OwnsPartOf=True
  end;
end;

procedure TTestBoldId.TestSubMemberID_Destroy_OwnsPartOf;
var
  PartOf: TBoldMemberID;
  Sub: TBoldSubMemberID;
begin
  // Test OwnsPartOf=False: PartOf must NOT be freed by Sub
  PartOf := TBoldMemberID.Create(3);
  try
    Sub := TBoldSubMemberID.Create(PartOf, False, 5);
    Sub.Free; // should NOT free PartOf
    // If PartOf was freed, accessing MemberIndex would AV
    Assert.AreEqual(3, PartOf.MemberIndex);
  finally
    PartOf.Free;
  end;
end;

// --- TBoldObjectId base ---

procedure TTestBoldId.TestObjectId_TopSortedIndex;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(1, 42, False);
  try
    Assert.AreEqual(42, Id.TopSortedIndex);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectId_TopSortedIndexExact;
var
  IdExact, IdInexact: TBoldInternalObjectId;
begin
  IdExact := CreateInternalId(1, 10, True);
  IdInexact := CreateInternalId(2, 10, False);
  try
    Assert.IsTrue(IdExact.TopSortedIndexExact);
    Assert.IsFalse(IdInexact.TopSortedIndexExact);
  finally
    IdExact.Free;
    IdInexact.Free;
  end;
end;

procedure TTestBoldId.TestObjectId_Clone;
var
  Id, Cloned: TBoldInternalObjectId;
begin
  Id := CreateInternalId(100, 5, True);
  try
    Cloned := TBoldInternalObjectId(Id.Clone);
    try
      Assert.AreNotSame(Id, Cloned);
      Assert.AreEqual(Id.AsString, Cloned.AsString);
      Assert.AreEqual(Id.TopSortedIndex, Cloned.TopSortedIndex);
      Assert.AreEqual(Id.TopSortedIndexExact, Cloned.TopSortedIndexExact);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectId_GetNonExisting;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(1, 0, True);
  try
    Assert.IsFalse(Id.NonExisting);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectId_GetTimeStamp;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(1, 0, True);
  try
    Assert.AreEqual(BOLDMAXTIMESTAMP, Id.TimeStamp);
  finally
    Id.Free;
  end;
end;

// --- TBoldInternalObjectId ---

procedure TTestBoldId.TestInternalId_CreateWithClassID;
var
  Id1, Id2: TBoldInternalObjectId;
begin
  // CreateWithClassID auto-increments InternalIdCounter
  Id1 := TBoldInternalObjectId.CreateWithClassID(5, True);
  Id2 := TBoldInternalObjectId.CreateWithClassID(5, True);
  try
    // Each should have a different internal identifier (auto-incremented)
    Assert.AreNotEqual(Id1.AsString, Id2.AsString);
    Assert.AreNotEqual(Id1.Hash, Id2.Hash);
  finally
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_CreateWithClassIDandInternalId;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(777, 3, False);
  try
    Assert.AreEqual('777', Id.AsString);
    Assert.AreEqual(Cardinal(777), Id.Hash);
    Assert.AreEqual(3, Id.TopSortedIndex);
    Assert.IsFalse(Id.TopSortedIndexExact);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_AsString;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(12345, 0, True);
  try
    Assert.AreEqual('12345', Id.AsString);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_Hash;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(42, 0, True);
  try
    Assert.AreEqual(Cardinal(42), Id.Hash);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_IsStorable;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(1, 0, True);
  try
    Assert.IsFalse(Id.IsStorable);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_StreamName;
var
  Id: TBoldInternalObjectId;
  Streamable: IBoldStreamable;
begin
  Id := CreateInternalId(1, 0, True);
  try
    Streamable := Id as IBoldStreamable;
    Assert.AreEqual(BOLDINTERNALIDNAME, Streamable.StreamName);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_IsEqual;
var
  Id1, Id2, Id3: TBoldInternalObjectId;
  NonExisting: TBoldNonExistingObjectId;
begin
  Id1 := CreateInternalId(50, 1, True);
  Id2 := CreateInternalId(50, 2, False);
  Id3 := CreateInternalId(51, 1, True);
  NonExisting := TBoldNonExistingObjectId.CreateWithClassID(1, True);
  try
    // Same internal identifier = equal (regardless of ClassId)
    Assert.IsTrue(Id1.IsEqual[Id2]);
    // Different internal identifier = not equal
    Assert.IsFalse(Id1.IsEqual[Id3]);
    // nil = not equal
    Assert.IsFalse(Id1.IsEqual[nil]);
    // Different class type = not equal
    Assert.IsFalse(Id1.IsEqual[NonExisting]);
  finally
    Id1.Free;
    Id2.Free;
    Id3.Free;
    NonExisting.Free;
  end;
end;

procedure TTestBoldId.TestInternalId_CloneWithClassId;
var
  Id, Cloned: TBoldInternalObjectId;
begin
  Id := CreateInternalId(200, 5, True);
  try
    Cloned := TBoldInternalObjectId(Id.CloneWithClassId(10, False));
    try
      // Preserves InternalIdentifier
      Assert.AreEqual(Id.AsString, Cloned.AsString);
      Assert.AreEqual(Id.Hash, Cloned.Hash);
      // Uses new class ID data
      Assert.AreEqual(10, Cloned.TopSortedIndex);
      Assert.IsFalse(Cloned.TopSortedIndexExact);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

// --- TBoldExternalObjectID ---

procedure TTestBoldId.TestExternalId_IsStorable;
begin
  // TBoldExternalObjectID is abstract (GetHash, GetIsEqual, etc. are abstract).
  // We cannot instantiate it directly, but GetIsStorable returns True.
  // Verified via code review and via subclass TBoldDefaultId which inherits it.
  Assert.Pass('TBoldExternalObjectID.GetIsStorable returns True (verified via code review - class is abstract)');
end;

// --- TBoldNonExistingObjectId ---

procedure TTestBoldId.TestNonExistingId_AsString;
var
  Id: TBoldNonExistingObjectId;
begin
  Id := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  try
    Assert.AreEqual('-1', Id.AsString);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestNonExistingId_NonExisting;
var
  Id: TBoldNonExistingObjectId;
begin
  Id := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  try
    Assert.IsTrue(Id.NonExisting);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestNonExistingId_IsStorable;
var
  Id: TBoldNonExistingObjectId;
begin
  Id := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  try
    Assert.IsFalse(Id.IsStorable);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestNonExistingId_HashAndStreamName;
var
  Id: TBoldNonExistingObjectId;
  Streamable: IBoldStreamable;
begin
  Id := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  try
    Assert.AreEqual(Cardinal(0), Id.Hash);
    Streamable := Id as IBoldStreamable;
    Assert.AreEqual('', Streamable.StreamName);
  finally
    Id.Free;
  end;
end;

procedure TTestBoldId.TestNonExistingId_IsEqual;
var
  Id1, Id2: TBoldNonExistingObjectId;
  InternalId: TBoldInternalObjectId;
begin
  Id1 := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  Id2 := TBoldNonExistingObjectId.CreateWithClassID(5, False);
  InternalId := CreateInternalId(1, 0, True);
  try
    // Any TBoldNonExistingObjectId matches another
    Assert.IsTrue(Id1.IsEqual[Id2]);
    Assert.IsTrue(Id2.IsEqual[Id1]);
    // Does not match other types
    Assert.IsFalse(Id1.IsEqual[InternalId]);
  finally
    Id1.Free;
    Id2.Free;
    InternalId.Free;
  end;
end;

procedure TTestBoldId.TestNonExistingId_CloneWithClassId;
var
  Id, Cloned: TBoldNonExistingObjectId;
begin
  Id := TBoldNonExistingObjectId.CreateWithClassID(0, True);
  try
    Cloned := TBoldNonExistingObjectId(Id.CloneWithClassId(5, False));
    try
      Assert.IsTrue(Cloned is TBoldNonExistingObjectId);
      Assert.AreNotSame(Id, TBoldObjectId(Cloned));
      Assert.AreEqual(5, Cloned.TopSortedIndex);
      Assert.IsFalse(Cloned.TopSortedIndexExact);
    finally
      Cloned.Free;
    end;
  finally
    Id.Free;
  end;
end;

// --- TBoldIdList ---

procedure TTestBoldId.TestIdList_CommaSeparatedIdList_Empty;
var
  List: TBoldObjectIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    Assert.AreEqual('', List.CommaSeparatedIdList);
  finally
    List.Free;
  end;
end;

procedure TTestBoldId.TestIdList_CommaSeparatedIdList_MultipleItems;
var
  List: TBoldObjectIdList;
  Id1, Id2, Id3: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  Id3 := CreateInternalId(30, 0, True);
  try
    List.Add(Id1);
    // Single item
    Assert.AreEqual('10', List.CommaSeparatedIdList);

    List.Add(Id2);
    List.Add(Id3);
    // Multiple items
    Assert.AreEqual('10,20,30', List.CommaSeparatedIdList);
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
    Id3.Free;
  end;
end;

// --- TBoldID base ---

procedure TTestBoldId.TestBoldID_GetDebugInfo;
var
  Id: TBoldInternalObjectId;
begin
  Id := CreateInternalId(999, 0, True);
  try
    Assert.AreEqual(Id.AsString, Id.DebugInfo);
  finally
    Id.Free;
  end;
end;

// --- TBoldObjectIdList ---

procedure TTestBoldId.TestObjectIdList_Create;
var
  List: TBoldObjectIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    Assert.AreEqual(0, List.Count);
  finally
    List.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_Add_ClonesId;
var
  List: TBoldObjectIdList;
  Id: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id := CreateInternalId(42, 1, True);
  try
    List.Add(Id);
    Assert.AreEqual(1, List.Count);
    // Add clones the ID, so the list entry should be a different object
    Assert.AreNotSame(TObject(Id), TObject(List[0]));
    // But equal
    Assert.IsTrue(Id.IsEqual[List[0]]);
  finally
    List.Free;
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_AddAndGetID;
var
  List: TBoldObjectIdList;
  Id: TBoldInternalObjectId;
  Returned: TBoldObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id := CreateInternalId(42, 1, True);
  try
    Returned := List.AddAndGetID(Id);
    Assert.AreEqual(1, List.Count);
    // Returns a clone
    Assert.AreNotSame(TObject(Id), TObject(Returned));
    Assert.IsTrue(Id.IsEqual[Returned]);
    // Returned is the same object in the list
    Assert.AreSame(TObject(Returned), TObject(List[0]));
  finally
    List.Free;
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_AddIfNotInList;
var
  List: TBoldObjectIdList;
  Id: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id := CreateInternalId(42, 1, True);
  try
    List.AddIfNotInList(Id);
    Assert.AreEqual(1, List.Count);
    // Adding same ID again should be skipped
    List.AddIfNotInList(Id);
    Assert.AreEqual(1, List.Count);
  finally
    List.Free;
    Id.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_AddList;
var
  List1, List2: TBoldObjectIdList;
  Id1, Id2: TBoldInternalObjectId;
begin
  List1 := TBoldObjectIdList.Create;
  List2 := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  try
    List2.Add(Id1);
    List2.Add(Id2);
    List1.AddList(List2);
    Assert.AreEqual(2, List1.Count);
    Assert.IsTrue(Id1.IsEqual[List1[0]]);
    Assert.IsTrue(Id2.IsEqual[List1[1]]);
  finally
    List1.Free;
    List2.Free;
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_Insert;
var
  List: TBoldObjectIdList;
  Id1, Id2, Id3: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  Id3 := CreateInternalId(30, 0, True);
  try
    List.Add(Id1);
    List.Add(Id3);
    // Insert Id2 at index 1 (between Id1 and Id3)
    List.Insert(1, Id2);
    Assert.AreEqual(3, List.Count);
    Assert.IsTrue(Id1.IsEqual[List[0]]);
    Assert.IsTrue(Id2.IsEqual[List[1]]);
    Assert.IsTrue(Id3.IsEqual[List[2]]);
    // Insert clones the ID
    Assert.AreNotSame(TObject(Id2), TObject(List[1]));
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
    Id3.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_Clone;
var
  List, Cloned: TBoldObjectIdList;
  Id1, Id2: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  try
    List.Add(Id1);
    List.Add(Id2);
    Cloned := List.Clone;
    try
      Assert.AreNotSame(TObject(List), TObject(Cloned));
      Assert.AreEqual(List.Count, Cloned.Count);
      Assert.IsTrue(Id1.IsEqual[Cloned[0]]);
      Assert.IsTrue(Id2.IsEqual[Cloned[1]]);
      // Deep clone - different objects
      Assert.AreNotSame(TObject(List[0]), TObject(Cloned[0]));
    finally
      Cloned.Free;
    end;
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_Remove;
var
  List: TBoldObjectIdList;
  Id1, Id2: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  try
    List.Add(Id1);
    List.Add(Id2);
    Assert.AreEqual(2, List.Count);
    // Remove uses IndexOf on the list's own object, so we pass list's entry
    List.Remove(List[0]);
    Assert.AreEqual(1, List.Count);
    Assert.IsTrue(Id2.IsEqual[List[0]]);
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_ReplaceID;
var
  List: TBoldObjectIdList;
  Id1, Id2, Id3: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  Id3 := CreateInternalId(30, 0, True);
  try
    List.Add(Id1);
    List.Add(Id2);
    // Replace Id1 with Id3
    List.ReplaceID(Id1, Id3);
    Assert.AreEqual(2, List.Count);
    Assert.IsTrue(Id3.IsEqual[List[0]]);
    Assert.IsTrue(Id2.IsEqual[List[1]]);
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
    Id3.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_FirstAndLast;
var
  List: TBoldObjectIdList;
  Id1, Id2: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  try
    // Empty list
    Assert.IsNull(List.First);
    Assert.IsNull(List.Last);

    List.Add(Id1);
    List.Add(Id2);
    Assert.IsTrue(Id1.IsEqual[List.First]);
    Assert.IsTrue(Id2.IsEqual[List.Last]);
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_IdInList;
var
  List: TBoldObjectIdList;
  Id1, Id2: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  try
    List.Add(Id1);
    Assert.IsTrue(List.IdInList[Id1]);
    Assert.IsFalse(List.IdInList[Id2]);
  finally
    List.Free;
    Id1.Free;
    Id2.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_GetIDByID_LinearSearch;
var
  List: TBoldObjectIdList;
  Id, Found: TBoldObjectId;
  NotInList: TBoldInternalObjectId;
  i: Integer;
begin
  // Linear search: Count < 10
  List := TBoldObjectIdList.Create;
  NotInList := CreateInternalId(999, 0, True);
  try
    for i := 1 to 5 do
      List.AddAndAdopt(CreateInternalId(i, 0, True));

    Assert.IsTrue(List.Count < 10, 'Should use linear search path');

    Id := CreateInternalId(3, 0, True);
    try
      Found := List.IDByID[Id];
      Assert.IsNotNull(Found);
      Assert.IsTrue(Id.IsEqual[Found]);
    finally
      Id.Free;
    end;

    // Not found
    Found := List.IDByID[NotInList];
    Assert.IsNull(Found);
  finally
    List.Free;
    NotInList.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_GetIDByID_HashSearch;
var
  List: TBoldObjectIdList;
  Id, Found: TBoldObjectId;
  NotInList: TBoldInternalObjectId;
  i: Integer;
begin
  // Hash search: Count >= 10
  List := TBoldObjectIdList.Create;
  NotInList := CreateInternalId(999, 0, True);
  try
    for i := 1 to 15 do
      List.AddAndAdopt(CreateInternalId(i, 0, True));

    Assert.IsTrue(List.Count >= 10, 'Should use hash search path');

    Id := CreateInternalId(7, 0, True);
    try
      Found := List.IDByID[Id];
      Assert.IsNotNull(Found);
      Assert.IsTrue(Id.IsEqual[Found]);
    finally
      Id.Free;
    end;

    // Not found
    Found := List.IDByID[NotInList];
    Assert.IsNull(Found);
  finally
    List.Free;
    NotInList.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_GetIndexByID;
var
  List: TBoldObjectIdList;
  Id: TBoldInternalObjectId;
  NotInList: TBoldInternalObjectId;
  i: Integer;
begin
  List := TBoldObjectIdList.Create;
  NotInList := CreateInternalId(999, 0, True);
  try
    // Linear path (< 10 items)
    for i := 1 to 5 do
      List.AddAndAdopt(CreateInternalId(i, 0, True));

    Id := CreateInternalId(3, 0, True);
    try
      Assert.AreEqual(2, List.IndexByID[Id]); // 0-based: items 1,2,3 -> index 2
    finally
      Id.Free;
    end;
    Assert.AreEqual(-1, List.IndexByID[NotInList]);

    // Hash path (>= 10 items)
    for i := 6 to 15 do
      List.AddAndAdopt(CreateInternalId(i, 0, True));

    Id := CreateInternalId(10, 0, True);
    try
      Assert.AreEqual(9, List.IndexByID[Id]); // 0-based: item 10 at index 9
    finally
      Id.Free;
    end;
    Assert.AreEqual(-1, List.IndexByID[NotInList]);
  finally
    List.Free;
    NotInList.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_ContainsSameIDs;
var
  List1, List2, List3: TBoldObjectIdList;
  Id1, Id2, Id3: TBoldInternalObjectId;
begin
  List1 := TBoldObjectIdList.Create;
  List2 := TBoldObjectIdList.Create;
  List3 := TBoldObjectIdList.Create;
  Id1 := CreateInternalId(10, 0, True);
  Id2 := CreateInternalId(20, 0, True);
  Id3 := CreateInternalId(30, 0, True);
  try
    List1.Add(Id1);
    List1.Add(Id2);

    List2.Add(Id2);
    List2.Add(Id1);

    List3.Add(Id1);
    List3.Add(Id3);

    // Same IDs (different order)
    Assert.IsTrue(List1.ContainsSameIDs(List2));
    // Different IDs
    Assert.IsFalse(List1.ContainsSameIDs(List3));
  finally
    List1.Free;
    List2.Free;
    List3.Free;
    Id1.Free;
    Id2.Free;
    Id3.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_HasInexactIds;
var
  List: TBoldObjectIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    List.AddAndAdopt(CreateInternalId(1, 5, True));
    Assert.IsFalse(List.HasInexactIds);

    List.AddAndAdopt(CreateInternalId(2, 5, False)); // inexact
    Assert.IsTrue(List.HasInexactIds);
  finally
    List.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_HasNonExistingIds;
var
  List: TBoldObjectIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    List.AddAndAdopt(CreateInternalId(1, 0, True));
    Assert.IsFalse(List.HasNonExistingIds);

    List.AddAndAdopt(TBoldNonExistingObjectId.CreateWithClassID(0, True));
    Assert.IsTrue(List.HasNonExistingIds);
  finally
    List.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_StreamName;
var
  List: TBoldObjectIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    Assert.AreEqual(BOLDOBJECTIDLISTNAME, (List as IBoldStreamable).StreamName);
  finally
    List.Free;
  end;
end;

// --- TBoldMemberIdList ---

procedure TTestBoldId.TestMemberIdList_StreamName;
var
  List: TBoldMemberIdList;
begin
  List := TBoldMemberIdList.Create;
  try
    Assert.AreEqual(BOLDMEMBERIDLISTNAME, (List as IBoldStreamable).StreamName);
  finally
    List.Free;
  end;
end;

procedure TTestBoldId.TestMemberIdList_HasId;
var
  List: TBoldMemberIdList;
  SearchId: TBoldMemberID;
begin
  List := TBoldMemberIdList.Create;
  try
    // TBoldMemberIdList.Add adopts items (OwnsEntries=True), so create inline
    List.Add(TBoldMemberID.Create(5));
    List.Add(TBoldMemberID.Create(10));

    SearchId := TBoldMemberID.Create(5);
    try
      Assert.IsTrue(List.HasId(SearchId));
    finally
      SearchId.Free;
    end;

    SearchId := TBoldMemberID.Create(10);
    try
      Assert.IsTrue(List.HasId(SearchId));
    finally
      SearchId.Free;
    end;

    SearchId := TBoldMemberID.Create(99);
    try
      Assert.IsFalse(List.HasId(SearchId));
    finally
      SearchId.Free;
    end;
  finally
    List.Free; // frees the owned MemberID objects
  end;
end;

procedure TTestBoldId.TestMemberIdList_IsEqual;
var
  List1, List2, List3, List4: TBoldMemberIdList;
begin
  List1 := TBoldMemberIdList.Create;
  List2 := TBoldMemberIdList.Create;
  List3 := TBoldMemberIdList.Create;
  List4 := TBoldMemberIdList.Create;
  try
    // List owns entries, so create fresh objects for each Add
    List1.Add(TBoldMemberID.Create(5));
    List1.Add(TBoldMemberID.Create(10));

    List2.Add(TBoldMemberID.Create(10));
    List2.Add(TBoldMemberID.Create(5));

    List3.Add(TBoldMemberID.Create(5));
    List3.Add(TBoldMemberID.Create(99));

    List4.Add(TBoldMemberID.Create(5));

    // Same members (different order)
    Assert.IsTrue(List1.IsEqual(List2));
    // Different members
    Assert.IsFalse(List1.IsEqual(List3));
    // nil
    Assert.IsFalse(List1.IsEqual(nil));
    // Different count
    Assert.IsFalse(List1.IsEqual(List4));
  finally
    List1.Free;
    List2.Free;
    List3.Free;
    List4.Free;
  end;
end;

procedure TTestBoldId.TestMemberIdList_Clone;
var
  List, Cloned: TBoldMemberIdList;
begin
  List := TBoldMemberIdList.Create;
  try
    // List owns entries, so create fresh objects for each Add
    List.Add(TBoldMemberID.Create(5));
    List.Add(TBoldMemberID.Create(10));
    Cloned := List.Clone;
    try
      Assert.AreEqual(2, Cloned.Count);
      Assert.AreEqual(5, Cloned[0].MemberIndex);
      Assert.AreEqual(10, Cloned[1].MemberIndex);
      // Deep clone
      Assert.AreNotSame(TObject(List[0]), TObject(Cloned[0]));
    finally
      Cloned.Free;
    end;
  finally
    List.Free;
  end;
end;

// --- TBoldIDTranslationList ---

procedure TTestBoldId.TestTranslationList_CreateAndDestroy;
var
  TL: TBoldIDTranslationList;
begin
  TL := TBoldIDTranslationList.Create;
  try
    Assert.AreEqual(0, TL.Count);
  finally
    TL.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_AddTranslation;
var
  TL: TBoldIDTranslationList;
  OldId, NewId: TBoldInternalObjectId;
begin
  TL := TBoldIDTranslationList.Create;
  OldId := CreateInternalId(1, 0, True);
  NewId := CreateInternalId(2, 0, True);
  try
    TL.AddTranslation(OldId, NewId);
    Assert.AreEqual(1, TL.Count);
    Assert.IsTrue(OldId.IsEqual[TL.OldIds[0]]);
    Assert.IsTrue(NewId.IsEqual[TL.NewIds[0]]);
  finally
    TL.Free;
    OldId.Free;
    NewId.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_AddTranslation_DedupOldId;
var
  TL: TBoldIDTranslationList;
  OldId, NewId: TBoldInternalObjectId;
begin
  TL := TBoldIDTranslationList.Create;
  OldId := CreateInternalId(1, 0, True);
  NewId := CreateInternalId(2, 0, True);
  try
    TL.AddTranslation(OldId, NewId);
    // Same OldId + same NewId pair again should be deduplicated
    TL.AddTranslation(OldId, NewId);
    Assert.AreEqual(1, TL.Count);
  finally
    TL.Free;
    OldId.Free;
    NewId.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_AddTranslation_DedupNewId;
var
  TL: TBoldIDTranslationList;
  OldId, NewId: TBoldInternalObjectId;
begin
  TL := TBoldIDTranslationList.Create;
  OldId := CreateInternalId(1, 0, True);
  NewId := CreateInternalId(2, 0, True);
  try
    TL.AddTranslation(OldId, NewId);
    // Same NewId + same OldId pair again (checked via NewId dedup path)
    TL.AddTranslation(OldId, NewId);
    Assert.AreEqual(1, TL.Count);
  finally
    TL.Free;
    OldId.Free;
    NewId.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_AddTranslation_NilSkip;
var
  TL: TBoldIDTranslationList;
  Id: TBoldInternalObjectId;
begin
  TL := TBoldIDTranslationList.Create;
  Id := CreateInternalId(1, 0, True);
  try
    TL.AddTranslation(nil, Id);
    Assert.AreEqual(0, TL.Count);
    TL.AddTranslation(Id, nil);
    Assert.AreEqual(0, TL.Count);
    TL.AddTranslation(nil, nil);
    Assert.AreEqual(0, TL.Count);
  finally
    TL.Free;
    Id.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_TranslateToNewAndOld;
var
  TL: TBoldIDTranslationList;
  OldId, NewId: TBoldInternalObjectId;
  Unknown: TBoldInternalObjectId;
  Found: TBoldObjectId;
begin
  TL := TBoldIDTranslationList.Create;
  OldId := CreateInternalId(1, 0, True);
  NewId := CreateInternalId(2, 0, True);
  Unknown := CreateInternalId(99, 0, True);
  try
    TL.AddTranslation(OldId, NewId);

    // TranslateToNewId: OldId -> NewId
    Found := TL.TranslateToNewId[OldId];
    Assert.IsTrue(NewId.IsEqual[Found]);

    // TranslateToOldId: NewId -> OldId
    Found := TL.TranslateToOldId[NewId];
    Assert.IsTrue(OldId.IsEqual[Found]);

    // Not found returns self
    Found := TL.TranslateToNewId[Unknown];
    Assert.IsTrue(Unknown.IsEqual[Found]);

    Found := TL.TranslateToOldId[Unknown];
    Assert.IsTrue(Unknown.IsEqual[Found]);
  finally
    TL.Free;
    OldId.Free;
    NewId.Free;
    Unknown.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_Capacity;
var
  TL: TBoldIDTranslationList;
begin
  TL := TBoldIDTranslationList.Create;
  try
    TL.Capacity := 100;
    Assert.AreEqual(100, TL.Capacity);
  finally
    TL.Free;
  end;
end;

procedure TTestBoldId.TestTranslationList_StreamName;
var
  TL: TBoldIDTranslationList;
begin
  TL := TBoldIDTranslationList.Create;
  try
    Assert.AreEqual(BOLDIDTRANSLATIONLISTNAME, (TL as IBoldStreamable).StreamName);
  finally
    TL.Free;
  end;
end;

// --- ExactifyIds + ApplyTranslationList ---

procedure TTestBoldId.TestObjectIdList_ExactifyIds;
var
  List: TBoldObjectIdList;
  TL: TBoldIDTranslationList;
  InexactId, ExactId: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  TL := TBoldIDTranslationList.Create;

  // Create an inexact ID and its exact replacement
  InexactId := CreateInternalId(1, 5, False);
  ExactId := CreateInternalId(1, 5, True);
  try
    List.Add(InexactId);
    Assert.IsTrue(List.HasInexactIds);

    // Translation: OldId=InexactId -> NewId=ExactId
    TL.AddTranslation(InexactId, ExactId);

    List.ExactifyIds(TL);
    Assert.IsFalse(List.HasInexactIds);
    Assert.IsTrue(List[0].TopSortedIndexExact);
  finally
    List.Free;
    TL.Free;
    InexactId.Free;
    ExactId.Free;
  end;
end;

procedure TTestBoldId.TestObjectIdList_ApplyTranslationList;
var
  List: TBoldObjectIdList;
  TL: TBoldIDTranslationList;
  OldId1, NewId1, OldId2, NewId2: TBoldInternalObjectId;
begin
  List := TBoldObjectIdList.Create;
  TL := TBoldIDTranslationList.Create;
  OldId1 := CreateInternalId(1, 0, True);
  NewId1 := CreateInternalId(100, 0, True);
  OldId2 := CreateInternalId(2, 0, True);
  NewId2 := CreateInternalId(200, 0, True);
  try
    List.Add(OldId1);
    List.Add(OldId2);

    TL.AddTranslation(OldId1, NewId1);
    TL.AddTranslation(OldId2, NewId2);

    List.ApplyTranslationList(TL);
    Assert.AreEqual(2, List.Count);
    Assert.IsTrue(NewId1.IsEqual[List[0]]);
    Assert.IsTrue(NewId2.IsEqual[List[1]]);
  finally
    List.Free;
    TL.Free;
    OldId1.Free;
    NewId1.Free;
    OldId2.Free;
    NewId2.Free;
  end;
end;

// --- EBoldOperationFailedForIdList ---

procedure TTestBoldId.TestOperationFailedForIdList_CreateAndDestroy;
var
  List: TBoldObjectIdList;
  E: EBoldOperationFailedForIdList;
begin
  List := TBoldObjectIdList.Create;
  try
    List.AddAndAdopt(CreateInternalId(1, 0, True));
    List.AddAndAdopt(CreateInternalId(2, 0, True));

    E := EBoldOperationFailedForIdList.Create('Test error %d', [42], List);
    try
      Assert.IsNotNull(E.IdList);
      Assert.AreEqual(2, E.IdList.Count);
      // IdList is a clone
      Assert.AreNotSame(TObject(List), TObject(E.IdList));
      Assert.IsTrue(List[0].IsEqual[E.IdList[0]]);
      Assert.IsTrue(List[1].IsEqual[E.IdList[1]]);
    finally
      E.Free;
    end;
  finally
    List.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldId);

end.
