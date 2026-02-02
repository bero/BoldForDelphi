unit Test.BoldCollections;

interface

uses
  System.Classes,
  DUnitX.TestFramework,
  BoldCollections;

type
  /// <summary>
  /// Test fixture for BoldCollections - uniquely named collection classes
  /// </summary>
  [TestFixture]
  [Category('Common')]
  TTestBoldCollections = class
  public
    [Test]
    [Category('Quick')]
    procedure TestCreateCollection;
    [Test]
    procedure TestAddItem;
    [Test]
    procedure TestGetItemByName;
    [Test]
    procedure TestGetItemByNameNotFound;
    [Test]
    procedure TestDuplicateNameRaises;
    [Test]
    procedure TestRenameItem;
    [Test]
    procedure TestGetDisplayName;
    [Test]
    procedure TestGetNamePathWithCollection;
    [Test]
    procedure TestGetNamePathWithoutCollection;
    [Test]
    procedure TestMultipleItems;
    [Test]
    procedure TestItemIndexLazyCreation;
    [Test]
    procedure TestUpdateClearsIndex;
  end;

  /// <summary>
  /// Concrete test item class with name storage
  /// </summary>
  TTestNamedItem = class(TBoldUniquelyNamedCollectionItemWithNameStorage)
  public
    property UniqueName;
  end;

  /// <summary>
  /// Test collection class
  /// </summary>
  TTestNamedCollection = class(TBoldCollectionWithUniquelyNamedItems)
  public
    constructor Create(AOwner: TPersistent);
    function Add: TTestNamedItem;
  end;

implementation

uses
  System.SysUtils,
  BoldDefs;

{ TTestNamedCollection }

constructor TTestNamedCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTestNamedItem);
end;

function TTestNamedCollection.Add: TTestNamedItem;
begin
  Result := inherited Add as TTestNamedItem;
end;

{ TTestBoldCollections }

procedure TTestBoldCollections.TestCreateCollection;
var
  Collection: TTestNamedCollection;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Assert.AreEqual(0, Collection.Count, 'New collection should be empty');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestAddItem;
var
  Collection: TTestNamedCollection;
  Item: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item := Collection.Add;
    Item.UniqueName := 'TestItem';
    Assert.AreEqual(1, Collection.Count, 'Should have one item');
    Assert.AreEqual('TestItem', Item.UniqueName, 'Item should have correct name');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestGetItemByName;
var
  Collection: TTestNamedCollection;
  Item, Found: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item := Collection.Add;
    Item.UniqueName := 'MyItem';

    Found := Collection.ItemByName['MyItem'] as TTestNamedItem;
    Assert.AreSame(Item, Found, 'Should find item by name');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestGetItemByNameNotFound;
var
  Collection: TTestNamedCollection;
  Found: TBoldUniquelyNamedCollectionItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Found := Collection.ItemByName['NonExistent'];
    Assert.IsNull(Found, 'Should return nil for non-existent name');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestDuplicateNameRaises;
var
  Collection: TTestNamedCollection;
  Item1, Item2: TTestNamedItem;
  ExceptionRaised: Boolean;
begin
  Collection := TTestNamedCollection.Create(nil);
  ExceptionRaised := False;
  try
    Item1 := Collection.Add;
    Item1.UniqueName := 'DuplicateName';

    Item2 := Collection.Add;
    try
      Item2.UniqueName := 'DuplicateName';
    except
      on E: EBold do
        ExceptionRaised := True;
    end;
    Assert.IsTrue(ExceptionRaised, 'Should raise EBold for duplicate name');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestRenameItem;
var
  Collection: TTestNamedCollection;
  Item: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item := Collection.Add;
    Item.UniqueName := 'OldName';

    // Rename the item
    Item.UniqueName := 'NewName';

    Assert.AreEqual('NewName', Item.UniqueName, 'Name should be updated');
    Assert.IsNull(Collection.ItemByName['OldName'], 'Old name should not be found');
    Assert.AreSame(Item, Collection.ItemByName['NewName'], 'New name should find item');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestGetDisplayName;
var
  Collection: TTestNamedCollection;
  Item: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item := Collection.Add;
    Item.UniqueName := 'DisplayTest';

    Assert.AreEqual('DisplayTest', Item.DisplayName, 'DisplayName should return UniqueName');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestGetNamePathWithCollection;
var
  Component: TComponent;
  Collection: TTestNamedCollection;
  Item: TTestNamedItem;
begin
  Component := TComponent.Create(nil);
  Component.Name := 'TestOwner';
  Collection := TTestNamedCollection.Create(Component);
  try
    Item := Collection.Add;
    Item.UniqueName := 'PathItem';

    // NamePath includes the collection's path and item name
    Assert.Contains(Item.GetNamePath, 'PathItem', 'NamePath should contain item name');
  finally
    Collection.Free;
    Component.Free;
  end;
end;

procedure TTestBoldCollections.TestGetNamePathWithoutCollection;
var
  Item: TTestNamedItem;
begin
  // Create item without adding to collection
  Item := TTestNamedItem.Create(nil);
  try
    Assert.AreEqual('TTestNamedItem', Item.GetNamePath, 'Should return ClassName when no collection');
  finally
    Item.Free;
  end;
end;

procedure TTestBoldCollections.TestMultipleItems;
var
  Collection: TTestNamedCollection;
  Item1, Item2, Item3: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item1 := Collection.Add;
    Item1.UniqueName := 'Alpha';

    Item2 := Collection.Add;
    Item2.UniqueName := 'Beta';

    Item3 := Collection.Add;
    Item3.UniqueName := 'Gamma';

    Assert.AreEqual(3, Collection.Count, 'Should have three items');
    Assert.AreSame(Item1, Collection.ItemByName['Alpha'], 'Should find Alpha');
    Assert.AreSame(Item2, Collection.ItemByName['Beta'], 'Should find Beta');
    Assert.AreSame(Item3, Collection.ItemByName['Gamma'], 'Should find Gamma');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestItemIndexLazyCreation;
var
  Collection: TTestNamedCollection;
  Item: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    // Add item before index is accessed
    Item := Collection.Add;
    Item.UniqueName := 'LazyItem';

    // First access creates the index
    Assert.AreSame(Item, Collection.ItemByName['LazyItem'], 'Should find item after lazy index creation');
  finally
    Collection.Free;
  end;
end;

procedure TTestBoldCollections.TestUpdateClearsIndex;
var
  Collection: TTestNamedCollection;
  Item1, Item2: TTestNamedItem;
begin
  Collection := TTestNamedCollection.Create(nil);
  try
    Item1 := Collection.Add;
    Item1.UniqueName := 'First';

    // Access to create index
    Assert.AreSame(Item1, Collection.ItemByName['First'], 'Should find first item');

    // Add another item (triggers Update which clears index)
    Item2 := Collection.Add;
    Item2.UniqueName := 'Second';

    // Index should be recreated and find both items
    Assert.AreSame(Item1, Collection.ItemByName['First'], 'Should still find first item');
    Assert.AreSame(Item2, Collection.ItemByName['Second'], 'Should find second item');
  finally
    Collection.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldCollections);

end.
