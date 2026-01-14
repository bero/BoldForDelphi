unit Test.BoldNamedValueList;

interface

uses
  DUnitX.TestFramework,
  BoldNamedValueList;

type
  [TestFixture]
  TTestBoldNamedValueList = class
  private
    FList: TBoldNamedValueList;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // TBoldNamedValueListEntry tests
    [Test]
    procedure TestEntryCreate;
    [Test]
    procedure TestEntryValueModification;
    [Test]
    procedure TestEntryObjectModification;

    // TBoldNamedValueList basic tests
    [Test]
    procedure TestListCreate;
    [Test]
    procedure TestAddEntry;
    [Test]
    procedure TestAddEntryWithObject;
    [Test]
    procedure TestItemByName;
    [Test]
    procedure TestValueByName;
    [Test]
    procedure TestSetValueByName_Existing;
    [Test]
    procedure TestSetValueByName_New;

    // Uncovered method tests
    [Test]
    procedure TestAddFromStrings;
    [Test]
    procedure TestAddFromStrings_WithObjects;
    [Test]
    procedure TestGetCommaText;
    [Test]
    procedure TestSetCommaText;
    [Test]
    procedure TestRemoveName_Existing;
    [Test]
    procedure TestRemoveName_NonExisting;
    [Test]
    procedure TestSetObjectByName_Existing;
    [Test]
    procedure TestSetObjectByName_New;
  end;

implementation

uses
  Classes,
  SysUtils;

{ TTestBoldNamedValueList }

procedure TTestBoldNamedValueList.Setup;
begin
  FList := TBoldNamedValueList.Create;
end;

procedure TTestBoldNamedValueList.TearDown;
begin
  FList.Free;
end;

procedure TTestBoldNamedValueList.TestEntryCreate;
var
  Entry: TBoldNamedValueListEntry;
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    Entry := TBoldNamedValueListEntry.Create('TestName', 'TestValue', Obj);
    try
      Assert.AreEqual('TestName', Entry.Name);
      Assert.AreEqual('TestValue', Entry.Value);
      Assert.AreSame(Obj, Entry.aObject);
    finally
      Entry.Free;
    end;
  finally
    Obj.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestEntryValueModification;
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := TBoldNamedValueListEntry.Create('Name', 'InitialValue', nil);
  try
    Assert.AreEqual('InitialValue', Entry.Value);
    Entry.Value := 'ModifiedValue';
    Assert.AreEqual('ModifiedValue', Entry.Value);
  finally
    Entry.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestEntryObjectModification;
var
  Entry: TBoldNamedValueListEntry;
  Obj1, Obj2: TObject;
begin
  Obj1 := TObject.Create;
  Obj2 := TObject.Create;
  try
    Entry := TBoldNamedValueListEntry.Create('Name', 'Value', Obj1);
    try
      Assert.AreSame(Obj1, Entry.aObject);
      Entry.aObject := Obj2;
      Assert.AreSame(Obj2, Entry.aObject);
    finally
      Entry.Free;
    end;
  finally
    Obj1.Free;
    Obj2.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestListCreate;
begin
  Assert.IsNotNull(FList);
  Assert.AreEqual(0, FList.Count);
end;

procedure TTestBoldNamedValueList.TestAddEntry;
var
  Entry: TBoldNamedValueListEntry;
begin
  Entry := FList.AddEntry('Key1', 'Value1');
  Assert.IsNotNull(Entry);
  Assert.AreEqual(1, FList.Count);
  Assert.AreEqual('Key1', Entry.Name);
  Assert.AreEqual('Value1', Entry.Value);
end;

procedure TTestBoldNamedValueList.TestAddEntryWithObject;
var
  Entry: TBoldNamedValueListEntry;
  Obj: TStringList;
begin
  Obj := TStringList.Create;
  // Note: OwnsEntries is true, but the Entry owns the object reference, not the list
  // We'll free Obj manually since the list doesn't own external objects
  Entry := FList.AddEntry('Key1', 'Value1', Obj);
  Assert.AreSame(Obj, Entry.aObject);
  // Clean up - the list owns the entry but not the external object
  Obj.Free;
end;

procedure TTestBoldNamedValueList.TestItemByName;
var
  Entry: TBoldNamedValueListEntry;
begin
  FList.AddEntry('First', 'Value1');
  FList.AddEntry('Second', 'Value2');
  FList.AddEntry('Third', 'Value3');

  Entry := FList.ItemByName['Second'];
  Assert.IsNotNull(Entry);
  Assert.AreEqual('Value2', Entry.Value);

  // Non-existing name should return nil
  Entry := FList.ItemByName['NonExisting'];
  Assert.IsNull(Entry);
end;

procedure TTestBoldNamedValueList.TestValueByName;
begin
  FList.AddEntry('Key1', 'Value1');
  FList.AddEntry('Key2', 'Value2');

  Assert.AreEqual('Value1', FList.ValueByName['Key1']);
  Assert.AreEqual('Value2', FList.ValueByName['Key2']);
  // Non-existing returns empty string
  Assert.AreEqual('', FList.ValueByName['NonExisting']);
end;

procedure TTestBoldNamedValueList.TestSetValueByName_Existing;
begin
  FList.AddEntry('Key1', 'OldValue');
  Assert.AreEqual('OldValue', FList.ValueByName['Key1']);

  FList.ValueByName['Key1'] := 'NewValue';
  Assert.AreEqual('NewValue', FList.ValueByName['Key1']);
  Assert.AreEqual(1, FList.Count);  // Should not create new entry
end;

procedure TTestBoldNamedValueList.TestSetValueByName_New;
begin
  Assert.AreEqual(0, FList.Count);

  FList.ValueByName['NewKey'] := 'NewValue';
  Assert.AreEqual(1, FList.Count);
  Assert.AreEqual('NewValue', FList.ValueByName['NewKey']);
end;

procedure TTestBoldNamedValueList.TestAddFromStrings;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Add('Name1=Value1');
    Strings.Add('Name2=Value2');
    Strings.Add('Name3=Value3');

    FList.AddFromStrings(Strings);

    Assert.AreEqual(3, FList.Count);
    Assert.AreEqual('Value1', FList.ValueByName['Name1']);
    Assert.AreEqual('Value2', FList.ValueByName['Name2']);
    Assert.AreEqual('Value3', FList.ValueByName['Name3']);
  finally
    Strings.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestAddFromStrings_WithObjects;
var
  Strings: TStringList;
  Obj1, Obj2: TObject;
begin
  Strings := TStringList.Create;
  Obj1 := TObject.Create;
  Obj2 := TObject.Create;
  try
    Strings.AddObject('Name1=Value1', Obj1);
    Strings.AddObject('Name2=Value2', Obj2);

    FList.AddFromStrings(Strings);

    Assert.AreEqual(2, FList.Count);
    Assert.AreSame(Obj1, FList.ObjectByName['Name1']);
    Assert.AreSame(Obj2, FList.ObjectByName['Name2']);
  finally
    Obj1.Free;
    Obj2.Free;
    Strings.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestGetCommaText;
var
  CommaText: string;
begin
  FList.AddEntry('Key1', 'Value1');
  FList.AddEntry('Key2', 'Value2');
  FList.AddEntry('Key3', 'Value3');

  CommaText := FList.CommaText;

  // CommaText format is "Key1=Value1,Key2=Value2,Key3=Value3"
  Assert.IsTrue(Pos('Key1=Value1', CommaText) > 0);
  Assert.IsTrue(Pos('Key2=Value2', CommaText) > 0);
  Assert.IsTrue(Pos('Key3=Value3', CommaText) > 0);
end;

procedure TTestBoldNamedValueList.TestSetCommaText;
begin
  FList.CommaText := 'Name1=Value1,Name2=Value2,Name3=Value3';

  Assert.AreEqual(3, FList.Count);
  Assert.AreEqual('Value1', FList.ValueByName['Name1']);
  Assert.AreEqual('Value2', FList.ValueByName['Name2']);
  Assert.AreEqual('Value3', FList.ValueByName['Name3']);
end;

procedure TTestBoldNamedValueList.TestRemoveName_Existing;
begin
  FList.AddEntry('Key1', 'Value1');
  FList.AddEntry('Key2', 'Value2');
  FList.AddEntry('Key3', 'Value3');
  Assert.AreEqual(3, FList.Count);

  FList.RemoveName('Key2');

  Assert.AreEqual(2, FList.Count);
  Assert.IsNull(FList.ItemByName['Key2']);
  Assert.IsNotNull(FList.ItemByName['Key1']);
  Assert.IsNotNull(FList.ItemByName['Key3']);
end;

procedure TTestBoldNamedValueList.TestRemoveName_NonExisting;
begin
  FList.AddEntry('Key1', 'Value1');
  Assert.AreEqual(1, FList.Count);

  // Should not raise exception, just do nothing
  FList.RemoveName('NonExisting');

  Assert.AreEqual(1, FList.Count);
end;

procedure TTestBoldNamedValueList.TestSetObjectByName_Existing;
var
  Obj1, Obj2: TObject;
begin
  Obj1 := TObject.Create;
  Obj2 := TObject.Create;
  try
    FList.AddEntry('Key1', 'Value1', Obj1);
    Assert.AreSame(Obj1, FList.ObjectByName['Key1']);

    FList.ObjectByName['Key1'] := Obj2;
    Assert.AreSame(Obj2, FList.ObjectByName['Key1']);
    Assert.AreEqual(1, FList.Count);  // Should not create new entry
  finally
    Obj1.Free;
    Obj2.Free;
  end;
end;

procedure TTestBoldNamedValueList.TestSetObjectByName_New;
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    Assert.AreEqual(0, FList.Count);

    FList.ObjectByName['NewKey'] := Obj;

    Assert.AreEqual(1, FList.Count);
    Assert.AreSame(Obj, FList.ObjectByName['NewKey']);
    // Value should be empty string when created via SetObjectByName
    Assert.AreEqual('', FList.ValueByName['NewKey']);
  finally
    Obj.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldNamedValueList);

end.
