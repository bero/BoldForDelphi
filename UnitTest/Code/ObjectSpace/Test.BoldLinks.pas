unit Test.BoldLinks;

{ DUnitX tests for BoldLinks - Multi-link controller functionality }
{ Tests DoPreChangeIfNeeded refactoring through multi-link operations }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldSystem,
  BoldSubscription,
  BoldElements,
  UndoTestModelClasses,
  maan_UndoRedoBase,
  maan_UndoRedoTestCaseUtils;

type
  [TestFixture]
  [Category('ObjectSpace')]
  TTestBoldLinks = class
  private
    FSubscriber: TLoggingSubscriber;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // Tests for direct multi-link operations (exercises DoPreChangeIfNeeded)
    [Test]
    [Category('Quick')]
    procedure TestDirectMultiLinkAdd;
    [Test]
    [Category('Quick')]
    procedure TestDirectMultiLinkAddMultiple;
    [Test]
    [Category('Quick')]
    procedure TestDirectMultiLinkRemove;
    [Test]
    [Category('Quick')]
    procedure TestDirectMultiLinkClear;

    // Tests for indirect multi-link operations (exercises DoPreChangeIfNeeded in SetFromIDLists)
    [Test]
    [Category('Quick')]
    procedure TestIndirectMultiLinkAdd;
    [Test]
    [Category('Quick')]
    procedure TestIndirectMultiLinkAddMultiple;
  end;

implementation

{ TTestBoldLinks }

procedure TTestBoldLinks.SetUp;
begin
  EnsureDM;
  FSubscriber := TLoggingSubscriber.Create;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldLinks.TearDown;
begin
  if dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
  end;
  FreeAndNil(FSubscriber);
end;

procedure TTestBoldLinks.TestDirectMultiLinkAdd;
var
  TransientObj: TATransientClass;
  PersistentObj: TAPersistentClass;
begin
  // Create objects - TATransientClass has 'many' multi-link to TAPersistentClass
  TransientObj := CreateATransientClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  // Verify initial state
  Assert.AreEqual(0, TransientObj.many.Count, 'Multi-link should be empty initially');

  // Add to multi-link - this exercises DoPreChangeIfNeeded internally
  PersistentObj.one := TransientObj;

  // Verify the link was established
  Assert.AreEqual(1, TransientObj.many.Count, 'Multi-link should have 1 item after add');
  Assert.AreSame(TObject(PersistentObj), TObject(TransientObj.many[0]), 'Multi-link should contain the added object');
end;

procedure TTestBoldLinks.TestDirectMultiLinkAddMultiple;
var
  TransientObj: TATransientClass;
  PersistentObj1, PersistentObj2, PersistentObj3: TAPersistentClass;
begin
  // Create objects
  TransientObj := CreateATransientClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj1 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj2 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj3 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  // Add multiple objects - each addition exercises DoPreChangeIfNeeded
  PersistentObj1.one := TransientObj;
  PersistentObj2.one := TransientObj;
  PersistentObj3.one := TransientObj;

  // Verify all links were established
  Assert.AreEqual(3, TransientObj.many.Count, 'Multi-link should have 3 items');
  Assert.IsTrue(TransientObj.many.Includes(PersistentObj1), 'Multi-link should contain PersistentObj1');
  Assert.IsTrue(TransientObj.many.Includes(PersistentObj2), 'Multi-link should contain PersistentObj2');
  Assert.IsTrue(TransientObj.many.Includes(PersistentObj3), 'Multi-link should contain PersistentObj3');
end;

procedure TTestBoldLinks.TestDirectMultiLinkRemove;
var
  TransientObj: TATransientClass;
  PersistentObj1, PersistentObj2: TAPersistentClass;
begin
  // Create and link objects
  TransientObj := CreateATransientClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj1 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj2 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  PersistentObj1.one := TransientObj;
  PersistentObj2.one := TransientObj;
  Assert.AreEqual(2, TransientObj.many.Count, 'Multi-link should have 2 items');

  // Remove one link - exercises DoPreChangeIfNeeded
  PersistentObj1.one := nil;

  // Verify removal
  Assert.AreEqual(1, TransientObj.many.Count, 'Multi-link should have 1 item after removal');
  Assert.IsFalse(TransientObj.many.Includes(PersistentObj1), 'Multi-link should not contain removed object');
  Assert.IsTrue(TransientObj.many.Includes(PersistentObj2), 'Multi-link should still contain PersistentObj2');
end;

procedure TTestBoldLinks.TestDirectMultiLinkClear;
var
  TransientObj: TATransientClass;
  PersistentObj1, PersistentObj2: TAPersistentClass;
begin
  // Create and link objects
  TransientObj := CreateATransientClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj1 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  PersistentObj2 := CreateAPersistentClass(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  PersistentObj1.one := TransientObj;
  PersistentObj2.one := TransientObj;
  Assert.AreEqual(2, TransientObj.many.Count, 'Multi-link should have 2 items');

  // Clear all links
  TransientObj.many.Clear;

  // Verify all links removed
  Assert.AreEqual(0, TransientObj.many.Count, 'Multi-link should be empty after clear');
  Assert.IsNull(PersistentObj1.one, 'PersistentObj1.one should be nil after clear');
  Assert.IsNull(PersistentObj2.one, 'PersistentObj2.one should be nil after clear');
end;

procedure TTestBoldLinks.TestIndirectMultiLinkAdd;
var
  Book: TBook;
  Topic: TTopic;
begin
  // Create objects - Book has 'Topic' indirect multi-link via topicbook link class
  Book := CreateBook(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  // Verify initial state
  Assert.AreEqual(0, Book.Topic.Count, 'Book.Topic multi-link should be empty initially');
  Assert.AreEqual(0, Topic.Book.Count, 'Topic.Book multi-link should be empty initially');

  // Add to indirect multi-link - this exercises DoPreChangeIfNeeded in SetFromIDLists
  Book.Topic.Add(Topic);

  // Verify the link was established on both ends
  Assert.AreEqual(1, Book.Topic.Count, 'Book.Topic should have 1 item');
  Assert.AreEqual(1, Topic.Book.Count, 'Topic.Book should have 1 item');
  Assert.AreSame(TObject(Topic), TObject(Book.Topic[0]), 'Book.Topic should contain the Topic');
  Assert.AreSame(TObject(Book), TObject(Topic.Book[0]), 'Topic.Book should contain the Book');
end;

procedure TTestBoldLinks.TestIndirectMultiLinkAddMultiple;
var
  Book: TBook;
  Topic1, Topic2, Topic3: TTopic;
begin
  // Create objects
  Book := CreateBook(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic1 := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic2 := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic3 := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  // Add multiple topics - each exercises DoPreChangeIfNeeded
  Book.Topic.Add(Topic1);
  Book.Topic.Add(Topic2);
  Book.Topic.Add(Topic3);

  // Verify all links established
  Assert.AreEqual(3, Book.Topic.Count, 'Book.Topic should have 3 items');
  Assert.IsTrue(Book.Topic.Includes(Topic1), 'Book.Topic should contain Topic1');
  Assert.IsTrue(Book.Topic.Includes(Topic2), 'Book.Topic should contain Topic2');
  Assert.IsTrue(Book.Topic.Includes(Topic3), 'Book.Topic should contain Topic3');

  // Verify reverse links
  Assert.AreEqual(1, Topic1.Book.Count, 'Topic1.Book should have 1 item');
  Assert.AreEqual(1, Topic2.Book.Count, 'Topic2.Book should have 1 item');
  Assert.AreEqual(1, Topic3.Book.Count, 'Topic3.Book should have 1 item');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLinks);

end.
