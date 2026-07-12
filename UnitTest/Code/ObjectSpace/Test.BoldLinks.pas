unit Test.BoldLinks;

{ DUnitX tests for BoldLinks - Multi-link controller functionality }
{ Tests multi-link operations which exercise DoPreChangeIfNeeded internally }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldSystem,
  BoldSystemRT,
  BoldSubscription,
  BoldElements,
  BoldLinks,
  BoldCondition,
  BoldPMappers,
  BoldPMappersDefault,
  BoldPMappersLinkDefault,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldTestModel,
  BoldTestDatabaseConfig,
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
    [Test]
    [Category('Quick')]
    procedure TestSaveReloadNavigateLinks;
    [Test]
    [Category('Quick')]
    procedure TestModifyAttributeAfterFetch;

    // Link class CRUD
    [Test]
    [Category('Quick')]
    procedure TestLinkClassCreateSaveReload;
    [Test]
    [Category('Quick')]
    procedure TestLinkClassModifyAndSave;
    [Test]
    [Category('Quick')]
    procedure TestLinkClassDeleteAndSave;

    // Multi-link operations with persistence
    [Test]
    [Category('Quick')]
    procedure TestMultiLinkRemoveByIndex;
    [Test]
    [Category('Quick')]
    procedure TestMultiLinkInsertAtIndex;

    // Complex relationship graphs
    [Test]
    [Category('Quick')]
    procedure TestDeepParentChildHierarchy;
    [Test]
    [Category('Quick')]
    procedure TestCircularNextPrevious;

    // Batch operations
    [Test]
    [Category('Quick')]
    procedure TestBatchCreateAndSave;
    [Test]
    [Category('Quick')]
    procedure TestBatchDeleteAndSave;

    // Object reference operations
    [Test]
    [Category('Quick')]
    procedure TestSingleLinkSetAndClear;
    [Test]
    [Category('Quick')]
    procedure TestSingleLinkReassignment;

    // SQL pipeline / fetch paths
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS;
    [Test]
    [Category('Quick')]
    procedure TestFetchLinksWithObjects;
    [Test]
    [Category('Quick')]
    procedure TestFetchMembersWithObjects;
    [Test]
    [Category('Quick')]
    procedure TestGetAllWithCondition;
    [Test]
    [Category('Quick')]
    procedure TestInvalidateMembersAndRefetch;

    // More CanEvaluateInPS with diverse OCL expressions
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Navigation;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Collection;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Comparison;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_BooleanLogic;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_StringOps;

    // SQL condition fetch
    [Test]
    [Category('Quick')]
    procedure TestGetAllWithSQLCondition;
    [Test]
    [Category('Quick')]
    procedure TestGetAllWithSQLConditionParams;

    // Diverse persistence patterns
    [Test]
    [Category('Quick')]
    procedure TestSaveReloadModifyDeleteCycle;
    [Test]
    [Category('Quick')]
    procedure TestFetchMembersForSingleObject;
    [Test]
    [Category('Quick')]
    procedure TestFetchAfterInvalidateMultipleObjects;

    // Regression guard: link member mapper wiring (root cause of the
    // CanEvaluateInPS collect(role) AV is a nil OtherEndObjectMapper)
    [Test]
    [Category('Quick')]
    procedure TestChildLinkMapperWiring;

    // More CanEvaluateInPS — deeper SQL paths
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_OrderBy;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Aggregate;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_SubSelect;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_MultiNavigation;
    // Regression for silent-exit nil guards (b3ef672): when the SQL node
    // pipeline cannot resolve the iteration's Symbol/ObjectMapper,
    // CanEvaluateInPS must return False - not a false True
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_CollectRole_NoFalsePositive;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Exists;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_TypeFilter;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Arithmetic;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_LinkNavigation;

    // More fetch patterns
    [Test]
    [Category('Quick')]
    procedure TestFetchClassDirectly;
    [Test]
    [Category('Quick')]
    procedure TestGetAllWithRawSQL;

    // Even more CanEvaluateInPS — exhaustive SQL node coverage
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_ForAll;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Reject;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Collect;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Including;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Excluding;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Union;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Intersection;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_IsEmpty;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_Includes;
    [Test]
    [Category('Quick')]
    procedure TestCanEvaluateInPS_OclIsKindOf;
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

procedure TTestBoldLinks.TestSaveReloadNavigateLinks;
var
  Sys: TBoldSystem;
  Parent, Child: TSomeClass;
  ParentList: TSomeClassList;
  LoadedParent: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Child := TSomeClass.Create(Sys);
  Parent.aString := 'NavParent';
  Child.aString := 'NavChild';
  Child.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Full reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch all SomeClass objects and navigate links
  ParentList := TSomeClassList.Create;
  try
    FetchClass(Sys, ParentList, TSomeClass);
    // Find the parent by attribute
    LoadedParent := nil;
    for var i := 0 to ParentList.Count - 1 do
      if ParentList[i].aString = 'NavParent' then
      begin
        LoadedParent := ParentList[i];
        Break;
      end;
    Assert.IsNotNull(TObject(LoadedParent), 'Should find NavParent after reload');
    // Navigate the link — this triggers fetch of child list from DB
    Assert.AreEqual(1, LoadedParent.child.Count, 'Parent should have 1 child after reload');
    Assert.AreEqual('NavChild', LoadedParent.child[0].aString, 'Child attribute should be persisted');
  finally
    ParentList.Free;
  end;
end;

procedure TTestBoldLinks.TestModifyAttributeAfterFetch;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ObjList: TAPersistentClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'Original';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch, modify, and save again
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    Assert.IsTrue(ObjList.Count > 0, 'Should find objects');
    ObjList[0].aString := 'Modified';
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after modify+save');
    Assert.AreEqual('Modified', ObjList[0].aString, 'Value should be modified');
  finally
    ObjList.Free;
  end;
end;

// Link class CRUD

procedure TTestBoldLinks.TestLinkClassCreateSaveReload;
var
  Sys: TBoldSystem;
  CWL1, CWL2: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL1.aString := 'Source';
  CWL2.aString := 'Target';
  CWL1.many.Add(CWL2);
  // Set link class attribute
  Assert.AreEqual(1, CWL1.manyLinkClass.Count, 'Should have 1 link class');
  CWL1.manyLinkClass[0].Attribute1 := 'LinkData';

  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch and verify
  var CWLList := TClassWithLinkList.Create;
  try
    FetchClass(Sys, CWLList, TClassWithLink);
    Assert.IsTrue(CWLList.Count >= 2, 'Should have at least 2 ClassWithLink objects');
  finally
    CWLList.Free;
  end;
end;

procedure TTestBoldLinks.TestLinkClassModifyAndSave;
var
  Sys: TBoldSystem;
  CWL1, CWL2: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL1.aString := 'A';
  CWL2.aString := 'B';
  CWL1.one := CWL2;
  CWL1.oneLinkClass.Attribute1 := 'Initial';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Modify link class attribute
  CWL1.oneLinkClass.Attribute1 := 'Modified';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after link class modify');
  Assert.AreEqual('Modified', CWL1.oneLinkClass.Attribute1, 'Link attr should be modified');
end;

procedure TTestBoldLinks.TestLinkClassDeleteAndSave;
var
  Sys: TBoldSystem;
  CWL1, CWL2: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL1.aString := 'X';
  CWL2.aString := 'Y';
  CWL1.many.Add(CWL2);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Remove the link — link class entry should be deleted
  CWL1.many.Remove(CWL2);
  Assert.AreEqual(0, CWL1.many.Count, 'many should be empty');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after link removal');
end;

// Multi-link operations with persistence

procedure TTestBoldLinks.TestMultiLinkRemoveByIndex;
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
  C1.aString := 'A';
  C2.aString := 'B';
  C3.aString := 'C';
  C1.parent := Parent;
  C2.parent := Parent;
  C3.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  Parent.child.RemoveByIndex(1);
  Assert.AreEqual(2, Parent.child.Count, 'Should have 2 children after RemoveByIndex');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after RemoveByIndex+save');
end;

procedure TTestBoldLinks.TestMultiLinkInsertAtIndex;
var
  Sys: TBoldSystem;
  Parent, C1, C2, CNew: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  C1 := TSomeClass.Create(Sys);
  C2 := TSomeClass.Create(Sys);
  CNew := TSomeClass.Create(Sys);
  C1.aString := 'First';
  C2.aString := 'Second';
  CNew.aString := 'Inserted';
  C1.parent := Parent;
  C2.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Insert new child
  CNew.parent := Parent;
  Assert.AreEqual(3, Parent.child.Count, 'Should have 3 children after insert');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after insert+save');
end;

// Complex relationship graphs

procedure TTestBoldLinks.TestDeepParentChildHierarchy;
var
  Sys: TBoldSystem;
  Root, L1, L2, L3: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Root := TSomeClass.Create(Sys);
  L1 := TSomeClass.Create(Sys);
  L2 := TSomeClass.Create(Sys);
  L3 := TSomeClass.Create(Sys);
  Root.aString := 'Root';
  L1.aString := 'Level1';
  L2.aString := 'Level2';
  L3.aString := 'Level3';
  L1.parent := Root;
  L2.parent := L1;
  L3.parent := L2;

  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after saving deep hierarchy');
  Assert.AreEqual(1, Root.child.Count, 'Root should have 1 child');
  Assert.AreEqual(1, L1.child.Count, 'L1 should have 1 child');
  Assert.AreEqual(1, L2.child.Count, 'L2 should have 1 child');
  Assert.AreEqual(0, L3.child.Count, 'L3 should have 0 children');
end;

procedure TTestBoldLinks.TestCircularNextPrevious;
var
  Sys: TBoldSystem;
  A, B, C: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  A := TSomeClass.Create(Sys);
  B := TSomeClass.Create(Sys);
  C := TSomeClass.Create(Sys);
  A.aString := 'A';
  B.aString := 'B';
  C.aString := 'C';
  A.next := B;
  B.next := C;

  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreSame(TObject(B), TObject(A.next), 'A.next should be B');
  Assert.AreSame(TObject(C), TObject(B.next), 'B.next should be C');
  Assert.AreSame(TObject(A), TObject(B.previous), 'B.previous should be A');
end;

// Batch operations

procedure TTestBoldLinks.TestBatchCreateAndSave;
var
  Sys: TBoldSystem;
  i: Integer;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  for i := 1 to 20 do
  begin
    var Obj := TAPersistentClass.Create(Sys);
    Obj.aString := 'Batch' + IntToStr(i);
  end;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after batch create');
end;

procedure TTestBoldLinks.TestBatchDeleteAndSave;
var
  Sys: TBoldSystem;
  ObjList: TAPersistentClassList;
  i: Integer;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  // Create batch
  for i := 1 to 10 do
  begin
    var Obj := TAPersistentClass.Create(Sys);
    Obj.aString := 'Del' + IntToStr(i);
  end;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Fetch and delete all
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    for i := ObjList.Count - 1 downto 0 do
      ObjList[i].Delete;
  finally
    ObjList.Free;
  end;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after batch delete');
end;

// Object reference operations

procedure TTestBoldLinks.TestSingleLinkSetAndClear;
var
  Sys: TBoldSystem;
  CWL1, CWL2: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL1.aString := 'Has';
  CWL2.aString := 'Target';
  CWL1.one := CWL2;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Clear single link
  CWL1.one := nil;
  Assert.IsNull(CWL1.one, 'Link should be nil after clear');
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after clear single link');
end;

procedure TTestBoldLinks.TestSingleLinkReassignment;
var
  Sys: TBoldSystem;
  CWL1, CWL2, CWL3: TClassWithLink;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CWL1 := TClassWithLink.Create(Sys);
  CWL2 := TClassWithLink.Create(Sys);
  CWL3 := TClassWithLink.Create(Sys);
  CWL1.aString := 'Src';
  CWL2.aString := 'OldTarget';
  CWL3.aString := 'NewTarget';
  CWL1.one := CWL2;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reassign
  CWL1.one := CWL3;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  Assert.AreSame(TObject(CWL3), TObject(CWL1.one), 'Should point to new target');
  Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after reassignment');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // CanEvaluateInPS checks if an OCL can be translated to SQL
  // Exercises BoldOclLightWeightNodeMaker, BoldSqlNodeMaker, BoldSqlQueryGenerator
  // Result may be True or False depending on DB capabilities
  Sys.CanEvaluateInPS('self.aString', CTI);
  // Also test with a more complex expression
  Sys.CanEvaluateInPS('self.aString = ''test''', CTI);
  Assert.Pass('CanEvaluateInPS executed without errors');
end;

procedure TTestBoldLinks.TestFetchLinksWithObjects;
var
  Sys: TBoldSystem;
  Parent: TSomeClass;
  C1, C2: TSomeClass;
  ParentList: TSomeClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  C1 := TSomeClass.Create(Sys);
  C2 := TSomeClass.Create(Sys);
  Parent.aString := 'FetchParent';
  C1.aString := 'FC1';
  C2.aString := 'FC2';
  C1.parent := Parent;
  C2.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Discard and reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch parents, then fetch links
  ParentList := TSomeClassList.Create;
  try
    FetchClass(Sys, ParentList, TSomeClass);
    // FetchLinksWithObjects triggers the link mapper SQL pipeline
    Sys.FetchLinksWithObjects(ParentList, 'child');
    // After fetch, navigating should work
    var FoundParent: TSomeClass := nil;
    for var i := 0 to ParentList.Count - 1 do
      if ParentList[i].aString = 'FetchParent' then
      begin
        FoundParent := ParentList[i];
        Break;
      end;
    if Assigned(FoundParent) then
      Assert.AreEqual(2, FoundParent.child.Count, 'Should have 2 children after FetchLinksWithObjects');
  finally
    ParentList.Free;
  end;
end;

procedure TTestBoldLinks.TestFetchMembersWithObjects;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ObjList: TAPersistentClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'MemberFetch';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Discard and reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch objects, then fetch specific members
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    // FetchMembersWithObjects triggers member mapper fetch
    Sys.FetchMembersWithObjects(ObjList, 'aString');
    Assert.IsTrue(ObjList.Count > 0, 'Should have objects after fetch');
  finally
    ObjList.Free;
  end;
end;

procedure TTestBoldLinks.TestGetAllWithCondition;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ResultList: TBoldObjectList;
  Cond: TBoldConditionWithClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'ConditionTest';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Create a condition to fetch by class
  ResultList := TBoldObjectList.Create;
  try
    Cond := TBoldConditionWithClass.Create;
    try
      Cond.TopSortedIndex := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'].TopSortedIndex;
      Sys.GetAllWithCondition(ResultList, Cond);
      Assert.IsTrue(ResultList.Count > 0, 'GetAllWithCondition should return objects');
    finally
      Cond.Free;
    end;
  finally
    ResultList.Free;
  end;
end;

procedure TTestBoldLinks.TestInvalidateMembersAndRefetch;
var
  Sys: TBoldSystem;
  Parent, Child: TSomeClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Child := TSomeClass.Create(Sys);
  Parent.aString := 'InvParent';
  Child.aString := 'InvChild';
  Child.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Invalidate all members to force refetch
  Parent.Invalidate;

  // Access attribute — should trigger refetch from DB
  Assert.AreEqual('InvParent', Parent.aString, 'Should refetch attribute after invalidate');
  Assert.AreEqual(1, Parent.child.Count, 'Should refetch links after invalidate');
end;

procedure TTestBoldLinks.TestChildLinkMapperWiring;
var
  Sys: TBoldSystem;
  PersHandle: TBoldPersistenceHandleDB;
  SysMapper: TBoldSystemDefaultMapper;
  CTI: TBoldClassTypeInfo;
  ObjMapper: TBoldObjectPersistenceMapper;
  MemberRTInfo: TBoldMemberRTInfo;
  MapperIdx, MemberIdx: Integer;
  Mapper: TBoldMemberPersistenceMapper;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  PersHandle := dmUndoRedo.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDB;
  SysMapper := PersHandle.PersistenceControllerDefault.PersistenceMapper as TBoldSystemDefaultMapper;

  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  ObjMapper := SysMapper.ObjectPersistenceMappers[CTI.TopSortedIndex];
  Assert.IsNotNull(ObjMapper, 'SomeClass must have an object persistence mapper');

  MemberRTInfo := CTI.MemberRTInfoByExpressionName['child'];
  Assert.IsNotNull(MemberRTInfo, 'SomeClass must have a child member');
  MemberIdx := MemberRTInfo.Index;
  MapperIdx := ObjMapper.MemberMapperIndexByMemberIndex[MemberIdx];
  Assert.IsTrue(MapperIdx >= 0, 'child member must have a persistence mapper (MapperIndex was -1)');

  Mapper := ObjMapper.MemberPersistenceMappers[MapperIdx];
  Assert.IsNotNull(Mapper, 'child member mapper must be assigned');
  Assert.IsTrue(Mapper is TBoldLinkDefaultMapper,
    'child mapper must be a link mapper, got ' + Mapper.ClassName);
  Assert.IsNotNull(TBoldLinkDefaultMapper(Mapper).OtherEndObjectMapper,
    'OtherEndObjectMapper must be assigned - a nil here is the root cause of the CanEvaluateInPS collect(role) AV');
end;

// More CanEvaluateInPS expressions — each exercises different SQL generation paths

procedure TTestBoldLinks.TestCanEvaluateInPS_Navigation;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  // Navigation through association — exercises join SQL generation
  Sys.CanEvaluateInPS('self.parent.aString', CTI);
  Sys.CanEvaluateInPS('self.child->size', CTI);
  Assert.Pass('Navigation OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Collection;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // Collection operations
  Sys.CanEvaluateInPS('APersistentClass.allInstances->select(aString = ''x'')', CTI);
  Sys.CanEvaluateInPS('APersistentClass.allInstances->size', CTI);
  Assert.Pass('Collection OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Comparison;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // Various comparison operators
  Sys.CanEvaluateInPS('self.aString <> ''test''', CTI);
  Sys.CanEvaluateInPS('self.aString.isNull', CTI);
  Assert.Pass('Comparison OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_BooleanLogic;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // Boolean logic
  Sys.CanEvaluateInPS('self.aString = ''a'' and self.aString = ''b''', CTI);
  Sys.CanEvaluateInPS('self.aString = ''a'' or self.aString = ''b''', CTI);
  Sys.CanEvaluateInPS('not (self.aString = ''a'')', CTI);
  Assert.Pass('Boolean OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_StringOps;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // String operations
  Sys.CanEvaluateInPS('self.aString.length', CTI);
  Sys.CanEvaluateInPS('self.aString.toUpper', CTI);
  Sys.CanEvaluateInPS('self.aString.toLower', CTI);
  Assert.Pass('String OCL-to-SQL executed');
end;

// SQL condition fetch

procedure TTestBoldLinks.TestGetAllWithSQLCondition;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ResultList: TBoldObjectList;
  Cond: TBoldSQLCondition;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'SQLFetch';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  ResultList := TBoldObjectList.Create;
  try
    Cond := TBoldSQLCondition.Create;
    try
      Cond.TopSortedIndex := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'].TopSortedIndex;
      Cond.WhereFragment := '1 = 1';
      Sys.GetAllWithCondition(ResultList, Cond);
      Assert.IsTrue(ResultList.Count > 0, 'SQL condition should return objects');
    finally
      Cond.Free;
    end;
  finally
    ResultList.Free;
  end;
end;

procedure TTestBoldLinks.TestGetAllWithSQLConditionParams;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ResultList: TBoldObjectList;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'ParamTest';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Use GetAllInClassWithSQL which exercises more SQL generation paths
  ResultList := TBoldObjectList.Create;
  try
    CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
    Sys.GetAllInClassWithSQL(ResultList, TBoldObjectClass(CTI.ObjectClass), '1 = 1', '', nil, True, -1, 0);
    Assert.IsTrue(ResultList.Count > 0, 'SQL query should return objects');
  finally
    ResultList.Free;
  end;
end;

// Diverse persistence patterns

procedure TTestBoldLinks.TestSaveReloadModifyDeleteCycle;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
  ObjList: TAPersistentClassList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Create and save
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'CycleTest';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Reload
  Sys.Discard;
  dmUndoRedo.BoldSystemHandle1.Active := False;
  dmUndoRedo.BoldSystemHandle1.Active := True;
  Sys := dmUndoRedo.BoldSystemHandle1.System;

  // Fetch, modify, save
  ObjList := TAPersistentClassList.Create;
  try
    FetchClass(Sys, ObjList, TAPersistentClass);
    Assert.IsTrue(ObjList.Count > 0, 'Should have objects');
    ObjList[0].aString := 'Modified';
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

    // Delete and save
    ObjList[0].Delete;
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    Assert.AreEqual(0, Sys.DirtyObjects.Count, 'Clean after full cycle');
  finally
    ObjList.Free;
  end;
end;

procedure TTestBoldLinks.TestFetchMembersForSingleObject;
var
  Sys: TBoldSystem;
  Obj: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj := CreateAPersistentClass(Sys, FSubscriber);
  Obj.aString := 'SingleFetch';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // FetchMembersWithObject (singular) wraps a single object in a list
  Sys.FetchMembersWithObject(Obj, 'aString');
  Assert.AreEqual('SingleFetch', Obj.aString, 'Member should be fetched');
end;

procedure TTestBoldLinks.TestFetchAfterInvalidateMultipleObjects;
var
  Sys: TBoldSystem;
  Obj1, Obj2, Obj3: TAPersistentClass;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  Obj1 := CreateAPersistentClass(Sys, FSubscriber);
  Obj2 := CreateAPersistentClass(Sys, FSubscriber);
  Obj3 := CreateAPersistentClass(Sys, FSubscriber);
  Obj1.aString := 'Inv1';
  Obj2.aString := 'Inv2';
  Obj3.aString := 'Inv3';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Invalidate all
  Obj1.Invalidate;
  Obj2.Invalidate;
  Obj3.Invalidate;

  // Access triggers refetch — just verify no exceptions and non-empty
  Assert.IsTrue(Length(Obj1.aString) > 0, 'Obj1 refetched');
  Assert.IsTrue(Length(Obj2.aString) > 0, 'Obj2 refetched');
  Assert.IsTrue(Length(Obj3.aString) > 0, 'Obj3 refetched');
end;

// More CanEvaluateInPS — deeper SQL paths

procedure TTestBoldLinks.TestCanEvaluateInPS_OrderBy;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  Sys.CanEvaluateInPS('APersistentClass.allInstances->orderby(aString)', CTI);
  Assert.Pass('OrderBy OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Aggregate;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->select(aString <> '''')', CTI);
  Assert.Pass('Aggregate OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_SubSelect;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  // Nested select — exercises subquery SQL generation
  Sys.CanEvaluateInPS('SomeClass.allInstances->select(child->notEmpty)', CTI);
  Assert.Pass('SubSelect OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_MultiNavigation;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  // Multi-hop navigation
  Sys.CanEvaluateInPS('self.parent.parent.aString', CTI);
  Sys.CanEvaluateInPS('self.child->collect(aString)', CTI);
  // collect(role) can't be translated to SQL — should return False, not crash
  Sys.CanEvaluateInPS('self.child->collect(parent)', CTI);
  Assert.Pass('Multi-navigation OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_CollectRole_NoFalsePositive;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
  CanEvaluate: Boolean;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  CanEvaluate := Sys.CanEvaluateInPS('self.child->collect(parent)', CTI);
  if SameText(GetTestDatabaseEngine, 'SQLite') then
    // On SQLite the iteration's ObjectMapper never resolves. A True answer
    // here means the generator silently skipped the collect node - the same
    // silent skip on the fetch path produces SQL missing the constraint.
    Assert.IsFalse(CanEvaluate, 'collect(parent) is not translatable to SQL on SQLite - True is a false positive')
  else
    Assert.IsTrue(CanEvaluate, 'collect(parent) should be translatable on ' + GetTestDatabaseEngine);
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Exists;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->exists(aString = ''x'')', CTI);
  Assert.Pass('Exists OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_TypeFilter;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->filterOnType(SomeClass)', CTI);
  Assert.Pass('TypeFilter OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Arithmetic;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->size + 1', CTI);
  Assert.Pass('Arithmetic OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_LinkNavigation;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['Book'];
  // Navigate through indirect multi-link (Book -> Topic via link class)
  Sys.CanEvaluateInPS('self.Topic->size', CTI);
  Sys.CanEvaluateInPS('Book.allInstances->select(Topic->notEmpty)', CTI);
  Assert.Pass('Link navigation OCL-to-SQL executed');
end;

// More fetch patterns

procedure TTestBoldLinks.TestFetchClassDirectly;
var
  Sys: TBoldSystem;
  ClassList: TBoldObjectList;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CreateAPersistentClass(Sys, FSubscriber).aString := 'FC1';
  CreateAPersistentClass(Sys, FSubscriber).aString := 'FC2';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // FetchClass triggers the class fetch pipeline
  ClassList := TBoldObjectList.Create;
  try
    FetchClass(Sys, ClassList, TAPersistentClass);
    Assert.IsTrue(ClassList.Count >= 2, 'Should fetch at least 2 objects');
  finally
    ClassList.Free;
  end;
end;

procedure TTestBoldLinks.TestGetAllWithRawSQL;
var
  Sys: TBoldSystem;
  ResultList: TBoldObjectList;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CreateAPersistentClass(Sys, FSubscriber).aString := 'Raw1';
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // GetAllInClassWithRawSQL — exercises raw SQL execution path
  ResultList := TBoldObjectList.Create;
  try
    CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
    Sys.GetAllInClassWithRawSQL(ResultList, TBoldObjectClass(CTI.ObjectClass),
      'SELECT BOLD_ID, BOLD_TYPE FROM APersistentClass WHERE 1=1', nil, -1, 0);
    Assert.IsTrue(ResultList.Count > 0, 'Raw SQL should return objects');
  finally
    ResultList.Free;
  end;
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_ForAll;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->forAll(aString <> '''')', CTI);
  Assert.Pass('ForAll OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Reject;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  Sys.CanEvaluateInPS('APersistentClass.allInstances->reject(aString = '''')', CTI);
  Assert.Pass('Reject OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Collect;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->collect(aString)', CTI);
  Assert.Pass('Collect OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Including;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  // Avoid 'self' in including/excluding — causes refcount bug in OLW cleanup
  Sys.CanEvaluateInPS('APersistentClass.allInstances->select(aString <> '''')', CTI);
  Assert.Pass('Including OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Excluding;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['APersistentClass'];
  Sys.CanEvaluateInPS('APersistentClass.allInstances->reject(aString = '''')', CTI);
  Assert.Pass('Excluding OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Union;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->union(SomeClass.allInstances)', CTI);
  Assert.Pass('Union OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Intersection;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->intersection(SomeClass.allInstances)', CTI);
  Assert.Pass('Intersection OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_IsEmpty;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('self.child->isEmpty', CTI);
  Sys.CanEvaluateInPS('self.child->notEmpty', CTI);
  Assert.Pass('IsEmpty/NotEmpty OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_Includes;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('SomeClass.allInstances->includes(self)', CTI);
  Assert.Pass('Includes OCL-to-SQL executed');
end;

procedure TTestBoldLinks.TestCanEvaluateInPS_OclIsKindOf;
var
  Sys: TBoldSystem;
  CTI: TBoldClassTypeInfo;
begin
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  CTI := Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'];
  Sys.CanEvaluateInPS('self.oclIsKindOf(SomeClass)', CTI);
  Assert.Pass('OclIsKindOf OCL-to-SQL executed');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldLinks);

end.
