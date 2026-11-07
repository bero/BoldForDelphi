unit Test.BoldSystemPersistent;

{ The TBoldSystem tests that need a persistent system. They run on the
  SQLite-backed BoldTestDM (dmBoldTest), whose database comes from FireDAC on
  Delphi and from Zeos under Free Pascal (BoldTestDMFireDAC, BoldTestDMZeos).
  Each test gets a fresh database: SetUp opens it and TearDown closes it,
  because the fixture-level hooks exist only on the Delphi side of
  BoldTestFramework so far. }

interface

uses
  BoldTestFramework,
  BoldSystem;

type
  // Uses the SQLite-backed BoldTestDM: the dirty list is only maintained on a
  // persistent system (MarkObjectDirty is a no-op without a persistence
  // controller), so these tests cannot run on TjehodmBoldTest.
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldSystemDirtyObjectsPersistent = class(TBoldTestCase)
  private
    function GetSystem: TBoldSystem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestDirtyObjectsByClassTypeInfoIncludesSubclasses;
  end;

  // Uses the SQLite-backed BoldTestDM: UpdateDatabase fires PreUpdate
  // subscribers before object destruction is delayed, so a subscriber that
  // deletes a never-persisted object from the batch frees it immediately,
  // leaving a locator with a nil BoldObject in the already-filtered update
  // list.
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldSystemPreUpdatePersistent = class(TBoldTestCase)
  private
    fObjectToDelete: TBoldObject;
    function GetSystem: TBoldSystem;
    procedure DeletePendingObjectOnPreUpdate(Sender: TObject);
    procedure DeletePendingObjectAndRaiseOnPreUpdate(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestPreUpdateSubscriberDeletesNewObjectInBatch;
    procedure TestPreUpdateSubscriberDeletesNewObjectAndRaises;
  end;

  // Uses the SQLite-backed BoldTestDM: when a superclass extent is already
  // current, MakeDbCurrent fills the subclass extent from it (FillFromClassList)
  // instead of fetching from the database. GitHub issue #102.
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldClassListFillFromSuperClass = class(TBoldTestCase)
  private
    function GetSystem: TBoldSystem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestSubclassListFilledFromLoadedSuperClassList;
  end;

  // Uses the SQLite-backed BoldTestDM: a subscriber re-dirties saved objects
  // from inside their own discard (the discard of a modified member sends
  // beValueInvalid), so the dirty list never empties on its own and
  // DiscardPersistent has to stop after a bounded number of passes.
  {$IFNDEF FPC}
  [Category('ObjectSpace')]
  {$ENDIF}
  TTestBoldSystemDiscardPersistentStall = class(TBoldTestCase)
  private
    function GetSystem: TBoldSystem;
    procedure DeleteSavedObjects(const AObjects: array of TBoldObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published

    procedure TestDiscardPersistent_ReDirtiedDuringDiscard_EscalatesThenGivesUp;
    procedure TestDiscard_ReDirtiedDuringDiscard_GivesUpWithoutEscalation;
    procedure TestDiscardPersistent_FanOutReDirty_TerminatesWithinBoundedPasses;
  end;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldSubscription,
  BoldSystemRT,
  dmBoldTest,
  BoldTestModel;

{ TTestBoldSystemDirtyObjectsPersistent }

procedure TTestBoldSystemDirtyObjectsPersistent.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
end;

procedure TTestBoldSystemDirtyObjectsPersistent.TearDown;
begin
  CloseBoldTestDM;
  inherited;
end;

function TTestBoldSystemDirtyObjectsPersistent.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

procedure TTestBoldSystemDirtyObjectsPersistent.TestDirtyObjectsByClassTypeInfoIncludesSubclasses;
var
  Book: TBook;
  DirtyList: TBoldObjectList;
  RootTypeInfo: TBoldClassTypeInfo;
begin
  GetSystem.Discard;
  try
    // A new object is dirty. Querying the dirty list by a superclass type
    // info must include instances of subclasses - the TBoldClassTypeInfo
    // overload of GetDirtyObjectsAsBoldList compared BoldType with '='
    // (exact class only), so dirty subclass instances were omitted, while
    // the TBoldObjectClass overload uses 'is' and includes them.
    Book := TBook.Create(GetSystem);

    RootTypeInfo := GetSystem.BoldSystemTypeInfo.RootClassTypeInfo;
    DirtyList := GetSystem.DirtyObjectsAsBoldListByClassTypeInfo[RootTypeInfo];
    try
      Assert.IsTrue(DirtyList.Includes(Book),
        'dirty subclass instance must be included when querying by a superclass type info');
      Assert.AreEqual(1, DirtyList.Count, 'exactly the one dirty object expected');
    finally
      DirtyList.Free;
    end;
  finally
    GetSystem.Discard;
  end;
end;

{ TTestBoldSystemPreUpdatePersistent }

procedure TTestBoldSystemPreUpdatePersistent.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
end;

procedure TTestBoldSystemPreUpdatePersistent.TearDown;
begin
  CloseBoldTestDM;
  inherited;
end;

function TTestBoldSystemPreUpdatePersistent.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

procedure TTestBoldSystemPreUpdatePersistent.DeletePendingObjectOnPreUpdate(Sender: TObject);
begin
  if Assigned(fObjectToDelete) then
  begin
    fObjectToDelete.Delete;
    fObjectToDelete := nil;
  end;
end;

procedure TTestBoldSystemPreUpdatePersistent.DeletePendingObjectAndRaiseOnPreUpdate(Sender: TObject);
begin
  DeletePendingObjectOnPreUpdate(Sender);
  raise EBold.Create('PreUpdate refused');
end;

procedure TTestBoldSystemPreUpdatePersistent.TestPreUpdateSubscriberDeletesNewObjectAndRaises;
var
  SavedBook: TBook;
  ErrorText: string;
begin
  GetSystem.Discard;
  SavedBook := TBook.Create(GetSystem);
  fObjectToDelete := TBook.Create(GetSystem);
  GetSystem.OnPreUpdate := DeletePendingObjectAndRaiseOnPreUpdate;
  try
    // The subscriber's exception must reach the caller, and the update must
    // not touch the deleted object's locator on the way out, although the
    // batch still holds it when the exception leaves PreUpdate.
    ErrorText := '';
    try
      GetSystem.UpdateDatabase;
    except
      on E: EBold do
        ErrorText := E.Message;
    end;
    Assert.AreEqual('PreUpdate refused', ErrorText);
    Assert.IsTrue(SavedBook.BoldObjectIsNew, 'nothing must be saved when PreUpdate raises');
  finally
    GetSystem.OnPreUpdate := nil;
    fObjectToDelete := nil;
    GetSystem.Discard;
  end;
end;

procedure TTestBoldSystemPreUpdatePersistent.TestPreUpdateSubscriberDeletesNewObjectInBatch;
var
  SavedBook: TBook;
begin
  GetSystem.Discard;
  SavedBook := TBook.Create(GetSystem);
  fObjectToDelete := TBook.Create(GetSystem);
  GetSystem.OnPreUpdate := DeletePendingObjectOnPreUpdate;
  try
    // Deleting a never-persisted object destroys it together with its
    // locator, which the update batch holds. The update must leave the object
    // out without reading freed memory and still save the surviving object.
    // The old code read the freed locator here; that shows only when freed
    // memory is overwritten, as under heaptrc (FPC) or FastMM FullDebugMode.
    GetSystem.UpdateDatabase;
    Assert.IsFalse(SavedBook.BoldDirty,
      'surviving object must be saved when a PreUpdate subscriber deletes a batch sibling');
  finally
    GetSystem.OnPreUpdate := nil;
    fObjectToDelete := nil;
    if SavedBook.BoldObjectIsNew then
      GetSystem.Discard
    else
    begin
      SavedBook.Delete;
      GetSystem.UpdateDatabase;
    end;
  end;
end;

{ TTestBoldClassListFillFromSuperClass }

procedure TTestBoldClassListFillFromSuperClass.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
end;

procedure TTestBoldClassListFillFromSuperClass.TearDown;
begin
  CloseBoldTestDM;
  inherited;
end;

function TTestBoldClassListFillFromSuperClass.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

procedure TTestBoldClassListFillFromSuperClass.TestSubclassListFilledFromLoadedSuperClassList;
var
  Book: TBook;
  Locator1, Locator2: TBoldObjectLocator;
  RootList, BookList: TBoldObjectList;
begin
  GetSystem.Discard;
  Book := TBook.Create(GetSystem);
  Book.Title := 'Book 1';
  Locator1 := Book.BoldObjectLocator;
  Book := TBook.Create(GetSystem);
  Book.Title := 'Book 2';
  Locator2 := Book.BoldObjectLocator;
  GetSystem.UpdateDatabase;
  try
    RootList := GetSystem.Classes[GetSystem.BoldSystemTypeInfo.RootClassTypeInfo.TopSortedIndex];
    BookList := GetSystem.ClassByExpressionName['Book'];

    // Cold start: neither extent is current and the objects are unloaded, so
    // the class events cannot have put the books into the Book extent.
    RootList.Invalidate;
    BookList.Invalidate;
    Locator1.UnloadBoldObject;
    Locator2.UnloadBoldObject;

    // Load the superclass extent first. An extent is current only once every
    // object in it is loaded, so fetch the objects as well as the ids.
    RootList.EnsureContentsCurrent;
    RootList.EnsureObjects;
    Assert.IsTrue(RootList.BoldPersistenceState = bvpsCurrent,
      'the root extent must be current once all its objects are loaded');
    Assert.IsTrue(RootList.LocatorInList(Locator1) and RootList.LocatorInList(Locator2),
      'the root extent must hold both books');

    // ... then the subclass extent, which is filled from the root extent, not
    // from the database.
    Assert.IsTrue(BookList.LocatorInList(Locator1),
      'book 1 missing from the Book extent filled from the loaded root extent');
    Assert.IsTrue(BookList.LocatorInList(Locator2),
      'book 2 missing from the Book extent filled from the loaded root extent');
    Assert.AreEqual(2, BookList.Count, 'Book extent filled from the loaded root extent');
  finally
    Locator1.EnsuredBoldObject.Delete;
    Locator2.EnsuredBoldObject.Delete;
    GetSystem.UpdateDatabase;
  end;
end;

type
  // Shared part of the re-dirty helpers: one subscriber for the discard
  // events, the fuse and the count. Redirty gets one call per event with a
  // value unique to that call. The fuse keeps an unguarded DiscardPersistent
  // finite, so a failing test fails instead of hanging the run.
  TRedirtyOnDiscardBase = class
  private
    fSubscriber: TBoldPassthroughSubscriber;
    fRedirtyCount: Integer;
    fMaxRedirties: Integer;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    procedure SubscribeTo(AMember: TBoldMember);
    procedure Redirty(Originator: TObject; const AValue: string); virtual; abstract;
  public
    constructor Create(AMaxRedirties: Integer);
    destructor Destroy; override;
    property RedirtyCount: Integer read fRedirtyCount;
  end;

  // One object: discarding aString sets aString again, so the object is dirty
  // again as soon as its discard has finished.
  TRedirtySelfOnDiscard = class(TRedirtyOnDiscardBase)
  private
    fObject: TSomeClass;
  protected
    procedure Redirty(Originator: TObject; const AValue: string); override;
  public
    constructor Create(AObject: TSomeClass; AMaxRedirties: Integer);
  end;

  // Hub and spokes: discarding the hub re-dirties both spokes, discarding a
  // spoke re-dirties the hub. The dirty count swings 1, 2, 1, 2, so a guard
  // that compares with the previous pass alone sees progress every second
  // pass and never trips.
  TFanOutRedirtyOnDiscard = class(TRedirtyOnDiscardBase)
  private
    fHub: TSomeClass;
    fSpoke1: TSomeClass;
    fSpoke2: TSomeClass;
  protected
    procedure Redirty(Originator: TObject; const AValue: string); override;
  public
    constructor Create(AHub, ASpoke1, ASpoke2: TSomeClass; AMaxRedirties: Integer);
  end;

{ TRedirtyOnDiscardBase }

constructor TRedirtyOnDiscardBase.Create(AMaxRedirties: Integer);
begin
  inherited Create;
  fMaxRedirties := AMaxRedirties;
  fSubscriber := TBoldPassthroughSubscriber.Create(Receive);
end;

destructor TRedirtyOnDiscardBase.Destroy;
begin
  FreeAndNil(fSubscriber);
  inherited;
end;

procedure TRedirtyOnDiscardBase.SubscribeTo(AMember: TBoldMember);
begin
  AMember.AddSmallSubscription(fSubscriber, [beValueInvalid], beValueInvalid);
end;

procedure TRedirtyOnDiscardBase.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if OriginalEvent <> beValueInvalid then
  begin
    Exit;
  end;

  if fRedirtyCount >= fMaxRedirties then
  begin
    Exit;
  end;

  Inc(fRedirtyCount);
  Redirty(Originator, 'r' + IntToStr(fRedirtyCount));
end;

{ TRedirtySelfOnDiscard }

constructor TRedirtySelfOnDiscard.Create(AObject: TSomeClass; AMaxRedirties: Integer);
begin
  inherited Create(AMaxRedirties);
  fObject := AObject;
  SubscribeTo(fObject.M_aString);
end;

procedure TRedirtySelfOnDiscard.Redirty(Originator: TObject; const AValue: string);
begin
  fObject.aString := AValue;
end;

{ TFanOutRedirtyOnDiscard }

constructor TFanOutRedirtyOnDiscard.Create(AHub, ASpoke1, ASpoke2: TSomeClass; AMaxRedirties: Integer);
begin
  inherited Create(AMaxRedirties);
  fHub := AHub;
  fSpoke1 := ASpoke1;
  fSpoke2 := ASpoke2;
  SubscribeTo(fHub.M_aString);
  SubscribeTo(fSpoke1.M_aString);
  SubscribeTo(fSpoke2.M_aString);
end;

procedure TFanOutRedirtyOnDiscard.Redirty(Originator: TObject; const AValue: string);
begin
  if Originator = TObject(fHub.M_aString) then
  begin
    fSpoke1.aString := AValue;
    fSpoke2.aString := AValue;
  end
  else
  begin
    fHub.aString := AValue;
  end;
end;

{ TTestBoldSystemDiscardPersistentStall }

const
  // Fuse for the re-dirty helpers: keeps an unguarded discard loop finite so
  // a failing test fails fast instead of hanging the test run.
  cMaxRedirties = 40;

procedure TTestBoldSystemDiscardPersistentStall.SetUp;
begin
  inherited;
  EnsureBoldTestDM;
end;

procedure TTestBoldSystemDiscardPersistentStall.TearDown;
begin
  CloseBoldTestDM;
  inherited;
end;

function TTestBoldSystemDiscardPersistentStall.GetSystem: TBoldSystem;
begin
  Result := BoldTestDM.BoldSystemHandle1.System;
end;

procedure TTestBoldSystemDiscardPersistentStall.DeleteSavedObjects(const AObjects: array of TBoldObject);
var
  i: Integer;
begin
  GetSystem.Discard;

  for i := Low(AObjects) to High(AObjects) do
  begin
    AObjects[i].Delete;
  end;

  GetSystem.UpdateDatabase;
end;

procedure TTestBoldSystemDiscardPersistentStall.TestDiscardPersistent_ReDirtiedDuringDiscard_EscalatesThenGivesUp;
const
  // The guard allows 3 passes without a new low in the dirty count, escalates
  // to a full discard for 3 more passes, then gives up. Each pass re-dirties
  // the object once, so 6 re-dirties are expected. Fewer means the guard gave
  // up without escalating; a count near the fuse means it never stopped the
  // loop.
  cMinExpectedRedirties = 6;
  cMaxAcceptedRedirties = 9;
var
  Obj: TSomeClass;
  Redirty: TRedirtySelfOnDiscard;
begin
  GetSystem.Discard;
  Obj := TSomeClass.Create(GetSystem);
  GetSystem.UpdateDatabase;
  Obj.aString := 'r0';

  Redirty := TRedirtySelfOnDiscard.Create(Obj, cMaxRedirties);
  try
    GetSystem.DiscardPersistent(False);

    Assert.IsTrue((Redirty.RedirtyCount >= cMinExpectedRedirties) and (Redirty.RedirtyCount <= cMaxAcceptedRedirties),
      Format('expected %d-%d re-dirties (3 stalled passes, escalation, 3 more), got %d (fuse %d)',
        [cMinExpectedRedirties, cMaxAcceptedRedirties, Redirty.RedirtyCount, cMaxRedirties]));

    FreeAndNil(Redirty);
    GetSystem.Discard;
    Assert.IsFalse(GetSystem.BoldDirty, 'a discard without re-dirtying must leave the system clean');
  finally
    Redirty.Free;
    DeleteSavedObjects([Obj]);
  end;
end;

procedure TTestBoldSystemDiscardPersistentStall.TestDiscard_ReDirtiedDuringDiscard_GivesUpWithoutEscalation;
const
  // Discard already drops transient links, so there is nothing to escalate
  // to: the guard gives up after 3 passes without a new low.
  cMinExpectedRedirties = 3;
  cMaxAcceptedRedirties = 5;
var
  Obj: TSomeClass;
  Redirty: TRedirtySelfOnDiscard;
begin
  GetSystem.Discard;
  Obj := TSomeClass.Create(GetSystem);
  GetSystem.UpdateDatabase;
  Obj.aString := 'r0';

  Redirty := TRedirtySelfOnDiscard.Create(Obj, cMaxRedirties);
  try
    GetSystem.Discard;

    Assert.IsTrue((Redirty.RedirtyCount >= cMinExpectedRedirties) and (Redirty.RedirtyCount <= cMaxAcceptedRedirties),
      Format('expected %d-%d re-dirties (3 stalled passes, no escalation), got %d (fuse %d)',
        [cMinExpectedRedirties, cMaxAcceptedRedirties, Redirty.RedirtyCount, cMaxRedirties]));
  finally
    Redirty.Free;
    DeleteSavedObjects([Obj]);
  end;
end;

procedure TTestBoldSystemDiscardPersistentStall.TestDiscardPersistent_FanOutReDirty_TerminatesWithinBoundedPasses;
const
  // The dirty count swings 1, 2, 1, 2 and never drops below its start, so the
  // guard escalates after 3 passes and gives up after 3 more: 6 passes, 9
  // re-dirty events (1 in a hub pass, 2 in a spoke pass). Fewer than 8 means
  // the guard gave up without escalating; a guard that compares with the
  // previous pass alone runs until the fuse.
  cMinExpectedRedirties = 8;
  cMaxAcceptedRedirties = 15;
var
  Hub, Spoke1, Spoke2: TSomeClass;
  Redirty: TFanOutRedirtyOnDiscard;
begin
  GetSystem.Discard;
  Hub := TSomeClass.Create(GetSystem);
  Spoke1 := TSomeClass.Create(GetSystem);
  Spoke2 := TSomeClass.Create(GetSystem);
  GetSystem.UpdateDatabase;
  Hub.aString := 'r0';

  Redirty := TFanOutRedirtyOnDiscard.Create(Hub, Spoke1, Spoke2, cMaxRedirties);
  try
    GetSystem.DiscardPersistent(False);

    Assert.IsTrue((Redirty.RedirtyCount >= cMinExpectedRedirties) and (Redirty.RedirtyCount <= cMaxAcceptedRedirties),
      Format('expected %d-%d re-dirties on a dirty count swinging 1, 2, 1, 2, got %d (fuse %d); '
        + 'progress must mean a new low, not a smaller count than the pass before',
        [cMinExpectedRedirties, cMaxAcceptedRedirties, Redirty.RedirtyCount, cMaxRedirties]));
  finally
    Redirty.Free;
    DeleteSavedObjects([Hub, Spoke1, Spoke2]);
  end;
end;

initialization
  BoldRegisterTest(TTestBoldSystemDirtyObjectsPersistent);
  BoldRegisterTest(TTestBoldSystemPreUpdatePersistent);
  BoldRegisterTest(TTestBoldClassListFillFromSuperClass);
  BoldRegisterTest(TTestBoldSystemDiscardPersistentStall);

end.
