unit Test.BoldAbstractSnooper;

{ DUnitX tests for BoldAbstractSnooper - OSS class-event generation (H11) }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldDefs,
  BoldMeta,
  BoldAbstractSnooper,
  Test.BoldAttributes;  // declares TjehodmBoldTest, the standard test datamodule

type
  [TestFixture]
  [Category('Persistence')]
  TTestBoldAbstractSnooper = class
  private
    FDataModule: TjehodmBoldTest;
    function FindClass(AModel: TMoldModel; const AName: string): TMoldClass;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    [Test]
    [Category('Quick')]
    procedure TestClassEventsCoverSuperclassChain;
    [Test]
    [Category('Quick')]
    procedure TestExcessMemberSlotsAreNotIndexedIntoModelMembers;
    [Test]
    [Category('Quick')]
    procedure TestNonEmbeddedEventNamesConcreteClassForInexactId;
  end;

implementation

uses
  BoldId,
  BoldCondition,
  BoldUpdatePrecondition,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldPersistenceControllerPassthrough,
  BoldFreeStandingValues,
  BoldDefaultStreamNames,
  BoldObjectSpaceExternalEvents,
  dmModel1;  // TestModel1: classes with embedded single links and subclasses

type
  // Stands in for the rest of the persistence chain: PMUpdate/ReserveNewIds
  // are absorbed, and PMExactifyIds resolves every inexact id to a configured
  // concrete class - the way the real chain resolves via BOLD_TYPE in the DB.
  TFakeExactifyingController = class(TBoldPersistenceControllerPassthrough)
  private
    fResolveToTopSortedIndex: integer;
    fExactifyCalls: integer;
  public
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    property ResolveToTopSortedIndex: integer read fResolveToTopSortedIndex write fResolveToTopSortedIndex;
    property ExactifyCalls: integer read fExactifyCalls;
  end;

  // AddEvent/AddClassEvents are protected; this collector exposes them for
  // the test without touching production visibility.
  TEventCollectingSnooper = class(TBoldAbstractSnooper)
  private
    fCollected: TStringList;
  protected
    procedure AddEvent(const AEvent: string); override;
  public
    constructor Create(MoldModel: TMoldModel); override;
    destructor Destroy; override;
    procedure TransmitEvents(const ClientID: TBoldClientID); override;
    procedure CallAddClassEvents(TopSortedIndex: integer);
    procedure CallNonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
    property Collected: TStringList read fCollected;
  end;

constructor TEventCollectingSnooper.Create(MoldModel: TMoldModel);
begin
  inherited Create(MoldModel);
  fCollected := TStringList.Create;
end;

destructor TEventCollectingSnooper.Destroy;
begin
  FreeAndNil(fCollected);
  inherited;
end;

procedure TEventCollectingSnooper.AddEvent(const AEvent: string);
begin
  fCollected.Add(AEvent);
end;

procedure TEventCollectingSnooper.TransmitEvents(const ClientID: TBoldClientID);
begin
  // not needed for these tests
end;

procedure TEventCollectingSnooper.CallAddClassEvents(TopSortedIndex: integer);
begin
  AddClassEvents(TopSortedIndex);
end;

procedure TEventCollectingSnooper.CallNonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
begin
  NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content, MoldClass);
end;

{ TFakeExactifyingController }

procedure TFakeExactifyingController.PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
var
  i: integer;
begin
  inc(fExactifyCalls);
  for i := 0 to ObjectIdList.Count - 1 do
    if not ObjectIdList[i].TopSortedIndexExact then
      TranslationList.AddTranslationAdoptNew(ObjectIdList[i],
        ObjectIdList[i].CloneWithClassId(fResolveToTopSortedIndex, true));
end;

procedure TFakeExactifyingController.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID);
begin
  // the DB layer is out of scope for these tests
end;

procedure TFakeExactifyingController.ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
begin
  // no new objects in these tests
end;

{ TTestBoldAbstractSnooper }

procedure TTestBoldAbstractSnooper.SetUp;
begin
  FDataModule := TjehodmBoldTest.Create(nil);
end;

procedure TTestBoldAbstractSnooper.TearDown;
begin
  FreeAndNil(FDataModule);
end;

function TTestBoldAbstractSnooper.FindClass(AModel: TMoldModel; const AName: string): TMoldClass;
var
  i: integer;
begin
  result := nil;
  for i := 0 to AModel.Classes.Count - 1 do
    if SameText(AModel.Classes[i].ExpandedExpressionName, AName) then
      exit(AModel.Classes[i]);
  Assert.Fail('class ' + AName + ' not found in test model');
end;

procedure TTestBoldAbstractSnooper.TestClassEventsCoverSuperclassChain;
var
  Snooper: TEventCollectingSnooper;
  MoldModel: TMoldModel;
  ClassB, ClassA: TMoldClass;
  EventB, EventA: string;

  function CountOf(const s: string): integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to Snooper.Collected.Count - 1 do
      if Snooper.Collected[i] = s then
        inc(result);
  end;

begin
  // Regression for H11: a change to ClassB must emit bsClassChanged for
  // ClassB AND its superclasses - clients watching a ClassA list contain the
  // changed ClassB instance too. The merged code emitted only the leaf event
  // while flagging the whole chain "already sent", so superclass watchers
  // never refreshed and a later direct ClassA change in the same batch was
  // dropped by the early-exit.
  MoldModel := FDataModule.BoldModel1.MoldModel;
  ClassB := FindClass(MoldModel, 'ClassB');
  ClassA := FindClass(MoldModel, 'ClassA');
  Assert.AreSame(TObject(ClassA), TObject(ClassB.SuperClass), 'model sanity: ClassB inherits ClassA');

  EventB := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, ClassB.ExpandedExpressionName, '', '', nil);
  EventA := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, ClassA.ExpandedExpressionName, '', '', nil);

  Snooper := TEventCollectingSnooper.Create(MoldModel);
  try
    Snooper.CallAddClassEvents(ClassB.TopSortedIndex);
    Assert.AreEqual(1, CountOf(EventB), 'leaf class event must be emitted');
    Assert.AreEqual(1, CountOf(EventA), 'superclass event must be emitted for the whole chain');

    // A direct superclass change later in the same batch: already covered by
    // the chain emission above - must stay at exactly one event (no drop, no dupe).
    Snooper.CallAddClassEvents(ClassA.TopSortedIndex);
    Assert.AreEqual(1, CountOf(EventA), 'superclass event neither dropped nor duplicated');
  finally
    Snooper.Free;
  end;
end;

procedure TTestBoldAbstractSnooper.TestExcessMemberSlotsAreNotIndexedIntoModelMembers;
var
  Snooper: TEventCollectingSnooper;
  MoldModel: TMoldModel;
  ClassA: TMoldClass;
  ValueSpace: TBoldFreeStandingValueSpace;
  Contents: IBoldObjectContents;
  ExcessValue: IBoldValue;
  Id: TBoldObjectId;
  ModelMemberCount: integer;
begin
  // Object contents can carry more member slots than the model class declares -
  // e.g. old values kept for an id that was re-issued to a class with fewer
  // members. The member loop sized itself from the contents but indexed
  // AllBoldMembers, so the surplus slot read past the end of the member list.
  // In production that raised only AFTER the update had committed: the caller
  // saw a failed save for data that was already written, and the whole batch of
  // OSS events was lost.
  MoldModel := FDataModule.BoldModel1.MoldModel;
  ClassA := FindClass(MoldModel, 'ClassA');
  ModelMemberCount := ClassA.AllBoldMembers.Count;

  ValueSpace := TBoldFreeStandingValueSpace.Create;
  try
    Id := TBoldInternalObjectId.CreateWithClassIDandInternalId(1, ClassA.TopSortedIndex, true);
    try
      Contents := ValueSpace.GetEnsuredObjectContentsByObjectId(Id);
      Assert.IsNotNull(Contents, 'contents for the test id must exist');

      // One slot beyond what the model class knows about.
      ExcessValue := Contents.EnsureMemberAndGetValueByIndex(ModelMemberCount, BoldContentName_Integer);
      Assert.IsNotNull(ExcessValue, 'excess member slot must be materialised');
      Assert.AreEqual(ModelMemberCount + 1, Contents.MemberCount, 'contents must carry one slot more than the model class');

      Snooper := TEventCollectingSnooper.Create(MoldModel);
      try
        try
          Snooper.CallNonEmbeddedStateOfObjectChanged(Contents, nil, ClassA);
        except
          on E: Exception do
            Assert.Fail(Format('surplus member slot must be ignored, not indexed into AllBoldMembers - got %s: %s',
              [E.ClassName, E.Message]));
        end;
        // ClassA is attribute-only (the model has no associations), so there is
        // no embedded single link to report on any of the real member slots.
        Assert.AreEqual(0, Snooper.Collected.Count, 'no link events expected for an attribute-only class');
      finally
        Snooper.Free;
      end;
    finally
      ExcessValue := nil;
      Contents := nil;
      Id.Free;
    end;
  finally
    ValueSpace.Free;
  end;
end;

procedure TTestBoldAbstractSnooper.TestNonEmbeddedEventNamesConcreteClassForInexactId;
var
  Snooper: TEventCollectingSnooper;
  Fake: TFakeExactifyingController;
  MoldModel: TMoldModel;
  ClassA, ItemClass, CompositeItem: TMoldClass;
  Role, OtherRole: TMoldRole;
  LinkIndex, j: integer;
  LinkName: string;
  OwnerId, LinkTargetId: TBoldObjectId;
  NewValues, OldValues: TBoldFreeStandingValueSpace;
  NewValuesIntf, OldValuesIntf: IBoldValueSpace;
  OwnerOldContents: IBoldObjectContents;
  LinkValue: IBoldValue;
  IdRef: IBoldObjectIdRef;
  IdList: TBoldObjectIdList;
  TimeStamp: TBoldTimeStampType;
  TimeOfLatestUpdate: TDateTime;
  ExpectedEvent, WrongEvent: string;
begin
  // An id read out of an embedded single-link value carries only the link's
  // declared class when that class has subclasses (inexact id). The OSS event
  // must name the object's concrete class - the receiver rebuilds an id from
  // the name and treats it as exact - so the snooper has to resolve inexact
  // ids through the persistence chain before naming, not echo the superclass.
  Ensuredm_Model;
  MoldModel := dm_Model1.BoldModel1.MoldModel;
  // production reaches the snooper only after BoldSystemRT has top-sorted the
  // model; without this, TopSortedIndex does not address MoldModel.Classes
  MoldModel.EnsureTopSorted;
  ClassA := FindClass(MoldModel, 'ClassA');
  ItemClass := FindClass(MoldModel, 'Item');
  CompositeItem := FindClass(MoldModel, 'CompositeItem');
  Assert.AreSame(TObject(ItemClass), TObject(CompositeItem.SuperClass), 'model sanity: CompositeItem inherits Item');

  // locate an embedded single link on ClassA and the link name the snooper
  // will report (the non-embedded other end)
  LinkIndex := -1;
  Role := nil;
  for j := 0 to ClassA.AllBoldMembers.Count - 1 do
    if (ClassA.AllBoldMembers[j] is TMoldRole) and
       TMoldRole(ClassA.AllBoldMembers[j]).EffectiveEmbedded and
       TMoldRole(ClassA.AllBoldMembers[j]).EffectivePersistent then
    begin
      LinkIndex := j;
      Role := TMoldRole(ClassA.AllBoldMembers[j]);
      break;
    end;
  Assert.IsTrue(LinkIndex >= 0, 'model sanity: ClassA must have an embedded single link');
  OtherRole := Role.OtherEnd;
  if OtherRole.RoleType = rtLinkRole then
    OtherRole := OtherRole.MainRole;
  LinkName := OtherRole.ExpandedExpressionName;

  NewValues := TBoldFreeStandingValueSpace.Create;
  OldValues := TBoldFreeStandingValueSpace.Create;
  IdList := TBoldObjectIdList.Create;
  OwnerId := TBoldInternalObjectId.CreateWithClassIDandInternalId(1, ClassA.TopSortedIndex, true);
  LinkTargetId := TBoldInternalObjectId.CreateWithClassIDandInternalId(42, ItemClass.TopSortedIndex, false);
  Snooper := nil;
  Fake := nil;
  try
    // old values: the link member held an id that is really a CompositeItem
    // but is only known as "some Item" (inexact - the declared class has
    // subclasses, so the FK column alone cannot say which one)
    OwnerOldContents := OldValues.GetEnsuredObjectContentsByObjectId(OwnerId);
    LinkValue := OwnerOldContents.EnsureMemberAndGetValueByIndex(LinkIndex, BoldContentName_ObjectIdRef);
    Assert.IsTrue(Supports(LinkValue, IBoldObjectIdRef, IdRef), 'link value must expose IBoldObjectIdRef');
    IdRef.SetFromId(LinkTargetId, false);

    // new values: the link is gone (cleared), so exactly one event is due
    NewValues.GetEnsuredObjectContentsByObjectId(OwnerId);

    IdList.Add(OwnerId);
    NewValues.GetInterface(IBoldValueSpace, NewValuesIntf);
    OldValues.GetInterface(IBoldValueSpace, OldValuesIntf);

    Snooper := TEventCollectingSnooper.Create(MoldModel);
    Fake := TFakeExactifyingController.Create;
    Fake.ResolveToTopSortedIndex := CompositeItem.TopSortedIndex;
    Snooper.NextPersistenceController := Fake;

    TimeStamp := 0;
    TimeOfLatestUpdate := 0;
    Snooper.PMUpdate(IdList, NewValuesIntf, OldValuesIntf, nil, nil, TimeStamp, TimeOfLatestUpdate, 0);

    ExpectedEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(
      bsNonEmbeddedStateOfObjectChanged, CompositeItem.ExpandedExpressionName, LinkName, '', LinkTargetId);
    WrongEvent := TBoldObjectSpaceExternalEvent.EncodeExternalEvent(
      bsNonEmbeddedStateOfObjectChanged, ItemClass.ExpandedExpressionName, LinkName, '', LinkTargetId);

    Assert.IsTrue(Snooper.Collected.IndexOf(ExpectedEvent) >= 0,
      Format('event must name the concrete class (want "%s", events: %s)',
        [ExpectedEvent, Snooper.Collected.CommaText]));
    Assert.IsTrue(Snooper.Collected.IndexOf(WrongEvent) < 0,
      'event must not name the declared superclass');
    Assert.IsTrue(Fake.ExactifyCalls > 0, 'snooper must resolve inexact ids through the chain');
  finally
    IdRef := nil;
    LinkValue := nil;
    OwnerOldContents := nil;
    NewValuesIntf := nil;
    OldValuesIntf := nil;
    Snooper.Free;
    Fake.Free;
    IdList.Free;
    OwnerId.Free;
    LinkTargetId.Free;
    NewValues.Free;
    OldValues.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldAbstractSnooper);

end.
