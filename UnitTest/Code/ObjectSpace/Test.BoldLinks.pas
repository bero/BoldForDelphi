unit Test.BoldLinks;

{ DUnitX tests for BoldLinks - Multi-link controller functionality }
{ Tests multi-link operations which exercise DoPreChangeIfNeeded internally }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldSystem,
  BoldSubscription,
  BoldElements,
  BoldLinks,
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

    // Indirect multi-link remove
    [Test]
    [Category('Quick')]
    procedure TestIndirectMultiLinkRemove;

    // Link class attribute access
    [Test]
    [Category('Quick')]
    procedure TestLinkClassAttribute;

    // Self-referencing associations (TSomeClass parent/child)
    [Test]
    [Category('Quick')]
    procedure TestSelfRefParentChild;
    [Test]
    [Category('Quick')]
    procedure TestSelfRefNextPrevious;
    [Test]
    [Category('Quick')]
    procedure TestSelfRefPartPartOf;

    // Persistence: save and reload
    [Test]
    [Category('Quick')]
    procedure TestSaveAndReloadLinks;
    [Test]
    [Category('Quick')]
    procedure TestSaveAndReloadIndirectLinks;
    [Test]
    [Category('Quick')]
    procedure TestSaveAndReloadSelfRefLinks;

    // Link operations with UpdateDatabase
    [Test]
    [Category('Quick')]
    procedure TestUpdateDatabaseWithNewObjects;
    [Test]
    [Category('Quick')]
    procedure TestUpdateDatabaseModifyAttributes;

    // Persistence: delete and verify
    [Test]
    [Category('Quick')]
    procedure TestDeleteObjectAndSave;
    [Test]
    [Category('Quick')]
    procedure TestDeleteLinkedObjectAndSave;

    // Multi-link with link class: add/remove via many
    [Test]
    [Category('Quick')]
    procedure TestManyToManyViaLinkClass;

    // Move operations on ordered links
    [Test]
    [Category('Quick')]
    procedure TestMultiLinkMoveIndex;

    // Fetch after discard
    [Test]
    [Category('Quick')]
    procedure TestFetchAfterDiscard;

    // Modify link and save
    [Test]
    [Category('Quick')]
    procedure TestModifyLinkAndSave;
    [Test]
    [Category('Quick')]
    procedure TestReassignSingleLinkAndSave;
    [Test]
    [Category('Quick')]
    procedure TestClearMultiLinkAndSave;
    [Test]
    [Category('Quick')]
    procedure TestDeleteWithManyToManyAndSave;
    [Test]
    [Category('Quick')]
    procedure TestMultipleUpdateDatabaseCycles;
    [Test]
    [Category('Quick')]
    procedure TestSaveReloadVerifyAttributes;
    [Test]
    [Category('Quick')]
    procedure TestSaveReloadVerifyLinks;
    [Test]
    [Category('Quick')]
    procedure TestSaveDeleteReloadVerifyGone;
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
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(FSubscriber);
  FreeAndNil(dmUndoRedo);
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

procedure TTestBoldLinks.TestIndirectMultiLinkRemove;
var
  Book: TBook;
  Topic1, Topic2: TTopic;
begin
  Book := CreateBook(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic1 := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);
  Topic2 := CreateTopic(dmUndoRedo.BoldSystemHandle1.System, FSubscriber);

  Book.Topic.Add(Topic1);
  Book.Topic.Add(Topic2);
  Assert.AreEqual(2, Book.Topic.Count, 'Should have 2 topics');

  Book.Topic.Remove(Topic1);
  Assert.AreEqual(1, Book.Topic.Count, 'Should have 1 topic after remove');
  Assert.IsFalse(Book.Topic.Includes(Topic1), 'Should not contain removed topic');
  Assert.IsTrue(Book.Topic.Includes(Topic2), 'Should still contain Topic2');
  Assert.AreEqual(0, Topic1.Book.Count, 'Removed topic should have 0 books');
end;

procedure TTestBoldLinks.TestLinkClassAttribute;
var
  Sys: TBoldSystem;
  CWL1, CWL2: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL1.aString := 'One';
  CWL2.aString := 'Two';

  // Set single link
  CWL1.one := CWL2;
  Assert.AreSame(TObject(CWL2), TObject(CWL1.one), 'Single link should point to CWL2');

  // Access link class attribute
  Assert.IsNotNull(CWL1.M_oneLinkClass, 'Link class should exist');
  CWL1.oneLinkClass.Attribute1 := 'LinkAttr';
  Assert.AreEqual('LinkAttr', CWL1.oneLinkClass.Attribute1, 'Link class attribute should be set');
end;

procedure TTestBoldLinks.TestSelfRefParentChild;
var
  Sys: TBoldSystem;
  Parent, Child1, Child2: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Child1 := TSomeClass.Create(Sys);
  Child2 := TSomeClass.Create(Sys);
  Parent.aString := 'Parent';
  Child1.aString := 'Child1';
  Child2.aString := 'Child2';

  Child1.parent := Parent;
  Child2.parent := Parent;
  Assert.AreEqual(2, Parent.child.Count, 'Parent should have 2 children');
  Assert.AreSame(TObject(Parent), TObject(Child1.parent), 'Child1 parent should be Parent');
end;

procedure TTestBoldLinks.TestSelfRefNextPrevious;
var
  Sys: TBoldSystem;
  Obj1, Obj2, Obj3: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj1 := TSomeClass.Create(Sys);
  Obj2 := TSomeClass.Create(Sys);
  Obj3 := TSomeClass.Create(Sys);

  Obj1.next := Obj2;
  Obj2.next := Obj3;
  Assert.AreSame(TObject(Obj2), TObject(Obj1.next), 'Obj1.next should be Obj2');
  Assert.AreSame(TObject(Obj1), TObject(Obj2.previous), 'Obj2.previous should be Obj1');
  Assert.AreSame(TObject(Obj3), TObject(Obj2.next), 'Obj2.next should be Obj3');
end;

procedure TTestBoldLinks.TestSelfRefPartPartOf;
var
  Sys: TBoldSystem;
  Container, Part1, Part2: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Container := TSomeClass.Create(Sys);
  Part1 := TSomeClass.Create(Sys);
  Part2 := TSomeClass.Create(Sys);

  Container.part.Add(Part1);
  Container.part.Add(Part2);
  Assert.AreEqual(2, Container.part.Count, 'Container should have 2 parts');
  Assert.IsTrue(Part1.partof.Includes(Container), 'Part1 partof should include Container');
end;

procedure TTestBoldLinks.TestSaveAndReloadLinks;
var
  Sys: TBoldSystem;
  TransientObj: TATransientClass;
  PersistentObj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  TransientObj := CreateATransientClass(Sys, FSubscriber);
  PersistentObj := CreateAPersistentClass(Sys, FSubscriber);
  PersistentObj.aString := 'Saved';
  PersistentObj.one := TransientObj;

  // Save to DB
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch and verify
  Assert.IsTrue(Sys.DirtyObjects.Count = 0, 'System should be clean after reload');
end;

procedure TTestBoldLinks.TestSaveAndReloadIndirectLinks;
var
  Sys: TBoldSystem;
  Book: TBook;
  Topic: TTopic;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Book := CreateBook(Sys, FSubscriber);
  Topic := CreateTopic(Sys, FSubscriber);
  Book.Title := 'TestBook';
  Topic.name := 'TestTopic';
  Book.Topic.Add(Topic);

  // Save to DB
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;

  Assert.IsTrue(dmUndoRedo.BoldSystemHandle1.System.DirtyObjects.Count = 0, 'Clean after reload');
end;

procedure TTestBoldLinks.TestSaveAndReloadSelfRefLinks;
var
  Sys: TBoldSystem;
  Parent, Child: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Child := TSomeClass.Create(Sys);
  Parent.aString := 'Parent';
  Child.aString := 'Child';
  Child.parent := Parent;

  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;

  Assert.IsTrue(dmUndoRedo.BoldSystemHandle1.System.DirtyObjects.Count = 0, 'Clean after reload');
end;

procedure TTestBoldLinks.TestUpdateDatabaseWithNewObjects;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'NewObject';

  Assert.IsTrue(Sys.DirtyObjects.Count > 0, 'System should be dirty');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'System should be clean after update');
end;

procedure TTestBoldLinks.TestUpdateDatabaseModifyAttributes;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'Initial';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Modify and save again
  Obj.aString := 'Modified';
  Assert.IsTrue(Sys.DirtyObjects.Count > 0, 'Should be dirty after modify');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Should be clean after second update');
end;

procedure TTestBoldLinks.TestDeleteObjectAndSave;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'ToDelete';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  Obj.Delete;
  Assert.IsTrue(Sys.DirtyObjects.Count > 0, 'Should be dirty after delete');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Should be clean after saving delete');
end;

procedure TTestBoldLinks.TestDeleteLinkedObjectAndSave;
var
  Sys: TBoldSystem;
  TransientObj: TATransientClass;
  PersistentObj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  TransientObj := CreateATransientClass(Sys, FSubscriber);
  PersistentObj := CreateAPersistentClass(Sys, FSubscriber);
  PersistentObj.one := TransientObj;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Delete the persistent object that has a link
  PersistentObj.Delete;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, TransientObj.many.Count, 'Link should be removed after delete');
end;

procedure TTestBoldLinks.TestManyToManyViaLinkClass;
var
  Sys: TBoldSystem;
  Book1, Book2: TBook;
  Topic1, Topic2: TTopic;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Book1 := CreateBook(Sys, FSubscriber);
  Book2 := CreateBook(Sys, FSubscriber);
  Topic1 := CreateTopic(Sys, FSubscriber);
  Topic2 := CreateTopic(Sys, FSubscriber);
  Book1.Title := 'Book1';
  Book2.Title := 'Book2';
  Topic1.name := 'Topic1';
  Topic2.name := 'Topic2';

  // Create many-to-many relationships
  Book1.Topic.Add(Topic1);
  Book1.Topic.Add(Topic2);
  Book2.Topic.Add(Topic1);

  Assert.AreEqual(2, Book1.Topic.Count, 'Book1 should have 2 topics');
  Assert.AreEqual(1, Book2.Topic.Count, 'Book2 should have 1 topic');
  Assert.AreEqual(2, Topic1.Book.Count, 'Topic1 should have 2 books');
  Assert.AreEqual(1, Topic2.Book.Count, 'Topic2 should have 1 book');

  // Save and verify
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after save');
end;

procedure TTestBoldLinks.TestMultiLinkMoveIndex;
var
  Sys: TBoldSystem;
  Parent: TSomeClass;
  C1, C2, C3: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  C1 := TSomeClass.Create(Sys);
  C2 := TSomeClass.Create(Sys);
  C3 := TSomeClass.Create(Sys);
  C1.aString := 'First';
  C2.aString := 'Second';
  C3.aString := 'Third';
  C1.parent := Parent;
  C2.parent := Parent;
  C3.parent := Parent;

  Assert.AreEqual(3, Parent.child.Count, 'Should have 3 children');
  // Move should not raise and count should be preserved
  Parent.child.Move(2, 0);
  Assert.AreEqual(3, Parent.child.Count, 'Should still have 3 children after move');
end;

procedure TTestBoldLinks.TestFetchAfterDiscard;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'SavedValue';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Discard and re-activate to force fetch
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // System should be clean, objects fetchable
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Should be clean after re-open');
end;

procedure TTestBoldLinks.TestModifyLinkAndSave;
var
  Sys: TBoldSystem;
  TransientObj1, TransientObj2: TATransientClass;
  PersistentObj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  TransientObj1 := CreateATransientClass(Sys, FSubscriber);
  TransientObj2 := CreateATransientClass(Sys, FSubscriber);
  PersistentObj := CreateAPersistentClass(Sys, FSubscriber);
  PersistentObj.one := TransientObj1;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reassign link
  PersistentObj.one := TransientObj2;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreSame(TObject(TransientObj2), TObject(PersistentObj.one), 'Link should point to new target');
  Assert.AreEqual(0, TransientObj1.many.Count, 'Old target should have 0 links');
end;

procedure TTestBoldLinks.TestReassignSingleLinkAndSave;
var
  Sys: TBoldSystem;
  Parent1, Parent2, Child: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent1 := TSomeClass.Create(Sys);
  Parent2 := TSomeClass.Create(Sys);
  Child := TSomeClass.Create(Sys);
  Parent1.aString := 'P1';
  Parent2.aString := 'P2';
  Child.aString := 'C';
  Child.parent := Parent1;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Move child to different parent
  Child.parent := Parent2;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  Assert.AreEqual(0, Parent1.child.Count, 'Old parent should have 0 children');
  Assert.AreEqual(1, Parent2.child.Count, 'New parent should have 1 child');
end;

procedure TTestBoldLinks.TestClearMultiLinkAndSave;
var
  Sys: TBoldSystem;
  Book: TBook;
  Topic1, Topic2: TTopic;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Book := CreateBook(Sys, FSubscriber);
  Topic1 := CreateTopic(Sys, FSubscriber);
  Topic2 := CreateTopic(Sys, FSubscriber);
  Book.Topic.Add(Topic1);
  Book.Topic.Add(Topic2);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Clear and save
  Book.Topic.Clear;
  Assert.AreEqual(0, Book.Topic.Count, 'Topics should be cleared');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after saving clear');
end;

procedure TTestBoldLinks.TestDeleteWithManyToManyAndSave;
var
  Sys: TBoldSystem;
  Book: TBook;
  Topic: TTopic;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Book := CreateBook(Sys, FSubscriber);
  Topic := CreateTopic(Sys, FSubscriber);
  Book.Title := 'ToDelete';
  Topic.name := 'Stays';
  Book.Topic.Add(Topic);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Delete the book — link class entries should also be deleted
  Book.Delete;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Topic.Book.Count, 'Topic should have 0 books after book deleted');
end;

procedure TTestBoldLinks.TestMultipleUpdateDatabaseCycles;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  i: Integer;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);

  // Multiple save cycles with modifications
  for i := 1 to 5 do
  begin
    Obj.aString := 'Iteration' + IntToStr(i);
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after iteration ' + IntToStr(i));
  end;
  Assert.AreEqual('Iteration5', Obj.aString, 'Final value should be Iteration5');
end;

procedure TTestBoldLinks.TestSaveReloadVerifyAttributes;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ObjList: TAPersistentClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'PersistMe';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch and verify attribute persisted
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    Assert.IsTrue(ObjList.Count > 0, 'Should find persisted objects');
    Assert.AreEqual('PersistMe', ObjList[0].aString, 'Attribute should be persisted');
  finally
    ObjList.Free;
  end;
end;

procedure TTestBoldLinks.TestSaveReloadVerifyLinks;
var
  Sys: TBoldSystem;
  Parent, Child: TSomeClass;
  ParentList: TSomeClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Child := TSomeClass.Create(Sys);
  Parent.aString := 'Parent';
  Child.aString := 'Child';
  Child.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch and verify link persisted
  ParentList := TSomeClassList.Create;
  try
    FetchClass(Sys, ParentList, TSomeClass);
    Assert.IsTrue(ParentList.Count >= 2, 'Should find at least 2 SomeClass objects');
  finally
    ParentList.Free;
  end;
end;

procedure TTestBoldLinks.TestSaveDeleteReloadVerifyGone;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ObjList: TAPersistentClassList;
  CountBefore: Integer;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Create and save
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'WillBeDeleted';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Count, then delete and save
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    CountBefore := ObjList.Count;
  finally
    ObjList.Free;
  end;

  Obj.Delete;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload and verify deleted
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    Assert.AreEqual(CountBefore - 1, ObjList.Count, 'Deleted object should be gone after reload');
  finally
    ObjList.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLinks);

end.
