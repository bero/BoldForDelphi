unit Test.BoldHashIndexes;

interface

uses
  DUnitX.TestFramework,
  Classes,
  BoldHashIndexes,
  BoldIndex,
  BoldIndexableList;

type
  // Helper list that exposes protected methods
  TTestableIndexableList = class(TBoldIndexableList)
  public
    procedure PublicSetIndexCapacity(Capacity: Integer);
    function PublicAddIndex(Index: TBoldIndex): Integer;
    procedure PublicSetIndexVariable(var IndexNo: Integer; NewIndexNo: Integer);
  end;

  // Test item with GUID key
  TGuidTestItem = class
  private
    FGuid: TGUID;
    FName: string;
  public
    constructor Create(const AGuid: TGUID; const AName: string);
    property Guid: TGUID read FGuid;
    property Name: string read FName;
  end;

  // Test index for GUID
  TTestGuidHashIndex = class(TBoldGUIDHashIndex)
  protected
    function ItemASKeyGUID(Item: TObject): TGUID; override;
  end;

  // Test item with Cardinal key
  TCardinalTestItem = class
  private
    FKey: Cardinal;
    FValue: string;
  public
    constructor Create(AKey: Cardinal; const AValue: string);
    property Key: Cardinal read FKey;
    property Value: string read FValue;
  end;

  // Test index for Cardinal
  TTestCardinalHashIndex = class(TBoldCardinalHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): Cardinal; override;
  public
    function PublicFindByCardinal(const KeyCardinal: Cardinal): TObject;
  end;

  [TestFixture]
  TTestBoldHashIndexes = class
  public
    // TBoldStringKey tests
    [Test]
    procedure TestHashString_CaseIndependent;

    // TBoldGUIDHashIndex tests
    [Test]
    procedure TestGuidHashIndex_AddAndFind;
    [Test]
    procedure TestGuidHashIndex_FindByGuid_NotFound;
    [Test]
    procedure TestGuidHashIndex_MultipleItems;

    // TBoldCardinalHashIndex tests
    [Test]
    procedure TestCardinalHashIndex_AddAndFind;
    [Test]
    procedure TestCardinalHashIndex_FindByCardinal_NotFound;
    [Test]
    procedure TestCardinalHashIndex_MultipleItems;
  end;

implementation

uses
  SysUtils;

{ TTestableIndexableList }

procedure TTestableIndexableList.PublicSetIndexCapacity(Capacity: Integer);
begin
  SetIndexCapacity(Capacity);
end;

function TTestableIndexableList.PublicAddIndex(Index: TBoldIndex): Integer;
begin
  Result := AddIndex(Index);
end;

procedure TTestableIndexableList.PublicSetIndexVariable(var IndexNo: Integer; NewIndexNo: Integer);
begin
  SetIndexVariable(IndexNo, NewIndexNo);
end;

{ TGuidTestItem }

constructor TGuidTestItem.Create(const AGuid: TGUID; const AName: string);
begin
  inherited Create;
  FGuid := AGuid;
  FName := AName;
end;

{ TTestGuidHashIndex }

function TTestGuidHashIndex.ItemASKeyGUID(Item: TObject): TGUID;
begin
  Result := TGuidTestItem(Item).Guid;
end;

{ TCardinalTestItem }

constructor TCardinalTestItem.Create(AKey: Cardinal; const AValue: string);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

{ TTestCardinalHashIndex }

function TTestCardinalHashIndex.ItemAsKeyCardinal(Item: TObject): Cardinal;
begin
  Result := TCardinalTestItem(Item).Key;
end;

function TTestCardinalHashIndex.PublicFindByCardinal(const KeyCardinal: Cardinal): TObject;
begin
  Result := FindByCardinal(KeyCardinal);
end;

{ TTestBoldHashIndexes }

procedure TTestBoldHashIndexes.TestHashString_CaseIndependent;
var
  Hash1, Hash2, Hash3: Cardinal;
begin
  // Test bscCaseIndependent mode
  Hash1 := TBoldStringKey.HashString('Hello', bscCaseIndependent);
  Hash2 := TBoldStringKey.HashString('HELLO', bscCaseIndependent);
  Hash3 := TBoldStringKey.HashString('hello', bscCaseIndependent);

  // All should produce the same hash
  Assert.AreEqual(Hash1, Hash2);
  Assert.AreEqual(Hash2, Hash3);
end;

procedure TTestBoldHashIndexes.TestGuidHashIndex_AddAndFind;
var
  List: TTestableIndexableList;
  Index: TTestGuidHashIndex;
  IndexNo: Integer;
  Item: TGuidTestItem;
  Guid1: TGUID;
  Found: TObject;
begin
  Guid1 := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');

  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestGuidHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item := TGuidTestItem.Create(Guid1, 'TestItem');
    List.Add(Item);

    Found := Index.FindByGUID(Guid1);
    Assert.IsNotNull(Found);
    Assert.AreSame(Item, Found);
  finally
    List.Free;
  end;
end;

procedure TTestBoldHashIndexes.TestGuidHashIndex_FindByGuid_NotFound;
var
  List: TTestableIndexableList;
  Index: TTestGuidHashIndex;
  IndexNo: Integer;
  Item: TGuidTestItem;
  Guid1, Guid2: TGUID;
  Found: TObject;
begin
  Guid1 := StringToGUID('{12345678-1234-1234-1234-123456789ABC}');
  Guid2 := StringToGUID('{AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE}');

  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestGuidHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item := TGuidTestItem.Create(Guid1, 'TestItem');
    List.Add(Item);

    // Search for non-existent GUID
    Found := Index.FindByGUID(Guid2);
    Assert.IsNull(Found);
  finally
    List.Free;
  end;
end;

procedure TTestBoldHashIndexes.TestGuidHashIndex_MultipleItems;
var
  List: TTestableIndexableList;
  Index: TTestGuidHashIndex;
  IndexNo: Integer;
  Item1, Item2, Item3: TGuidTestItem;
  Guid1, Guid2, Guid3: TGUID;
  Found: TObject;
begin
  Guid1 := StringToGUID('{11111111-1111-1111-1111-111111111111}');
  Guid2 := StringToGUID('{22222222-2222-2222-2222-222222222222}');
  Guid3 := StringToGUID('{33333333-3333-3333-3333-333333333333}');

  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestGuidHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item1 := TGuidTestItem.Create(Guid1, 'First');
    Item2 := TGuidTestItem.Create(Guid2, 'Second');
    Item3 := TGuidTestItem.Create(Guid3, 'Third');

    List.Add(Item1);
    List.Add(Item2);
    List.Add(Item3);

    Found := Index.FindByGUID(Guid1);
    Assert.AreSame(Item1, Found);

    Found := Index.FindByGUID(Guid2);
    Assert.AreSame(Item2, Found);

    Found := Index.FindByGUID(Guid3);
    Assert.AreSame(Item3, Found);
  finally
    List.Free;
  end;
end;

procedure TTestBoldHashIndexes.TestCardinalHashIndex_AddAndFind;
var
  List: TTestableIndexableList;
  Index: TTestCardinalHashIndex;
  IndexNo: Integer;
  Item: TCardinalTestItem;
  Found: TObject;
begin
  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestCardinalHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item := TCardinalTestItem.Create(12345, 'TestValue');
    List.Add(Item);

    Found := Index.PublicFindByCardinal(12345);
    Assert.IsNotNull(Found);
    Assert.AreSame(Item, Found);
  finally
    List.Free;
  end;
end;

procedure TTestBoldHashIndexes.TestCardinalHashIndex_FindByCardinal_NotFound;
var
  List: TTestableIndexableList;
  Index: TTestCardinalHashIndex;
  IndexNo: Integer;
  Item: TCardinalTestItem;
  Found: TObject;
begin
  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestCardinalHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item := TCardinalTestItem.Create(12345, 'TestValue');
    List.Add(Item);

    // Search for non-existent key
    Found := Index.PublicFindByCardinal(99999);
    Assert.IsNull(Found);
  finally
    List.Free;
  end;
end;

procedure TTestBoldHashIndexes.TestCardinalHashIndex_MultipleItems;
var
  List: TTestableIndexableList;
  Index: TTestCardinalHashIndex;
  IndexNo: Integer;
  Item1, Item2, Item3: TCardinalTestItem;
  Found: TObject;
begin
  List := TTestableIndexableList.Create;
  try
    List.PublicSetIndexCapacity(1);
    Index := TTestCardinalHashIndex.Create;
    IndexNo := -1;
    List.PublicSetIndexVariable(IndexNo, List.PublicAddIndex(Index));

    Item1 := TCardinalTestItem.Create(100, 'First');
    Item2 := TCardinalTestItem.Create(200, 'Second');
    Item3 := TCardinalTestItem.Create(300, 'Third');

    List.Add(Item1);
    List.Add(Item2);
    List.Add(Item3);

    Found := Index.PublicFindByCardinal(100);
    Assert.AreSame(Item1, Found);

    Found := Index.PublicFindByCardinal(200);
    Assert.AreSame(Item2, Found);

    Found := Index.PublicFindByCardinal(300);
    Assert.AreSame(Item3, Found);
  finally
    List.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldHashIndexes);

end.
