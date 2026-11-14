unit Test.BoldDataSet;

{ DUnitX tests for TBoldDataSet, the TDataSet over a Bold list handle (#117
  step 9). TBoldDataSet is the bridge that lets FMX (through LiveBindings) and
  Lazarus data-aware controls show Bold objects.

  Every Delphi target: without BOLD_VCL (Linux, macOS, or BOLD_NO_VCL on
  Windows) TBoldDataSet and its control packs link no VCL unit. Not in the
  Free Pascal runner yet: TBoldDataSet overrides Delphi's TDataSet API. }

interface

uses
  DB,
  BoldTestFramework,
  BoldControlPack,
  BoldStringControlPack,
  BoldDataSet,
  BoldHandles,
  BoldSystemHandle,
  BoldListHandle,
  // TestModel1 before jehoBCBoldTest: both declare TClassA, and the
  // unqualified TClassA here is jehoBCBoldTest's (the later unit wins)
  TestModel1,
  jehoBCBoldTest,
  jehoBoldTestDataModule;

type
  {$IFNDEF FPC}
  [Category('Handles')]
  {$ENDIF}
  TTestBoldDataSet = class(TBoldTestCase)
  private
    FDataModule: TjehodmBoldTest;
    FDataSet: TBoldDataSet;
    procedure AddField(const AExpression: string);
    function ListObject(AIndex: Integer): TClassA;
    procedure ChangeTwiceOnTheBoldSide(ATarget: TClassA);
    function WaitForFieldValue(const AFieldName, AExpected: string; ATimeoutMs: Integer): Boolean;
    procedure ProcessQueuedCallsFor(AMilliseconds: Integer);
  private
    // TestChangeOfAnotherRowWhileARowIsRead
    FHookArmed: Boolean;
    FHookTarget: TClassA;
    FHookRenderer: TBoldAsStringRenderer;
    FRefreshes: Integer;
    function ChangeAnotherRowWhileACellIsCreated(aFollower: TBoldFollower): string;
    procedure CountRefresh(Sender: TObject; Field: TField);
    function RefreshesWhileWaiting(AMilliseconds: Integer): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOpenShowsOneRecordPerObject;
    procedure TestFieldsReadTheAttributes;
    procedure TestNavigationFollowsTheListOrder;
    procedure TestActiveBoldObjectFollowsTheCursor;
    procedure TestPostWritesToTheObject;
    procedure TestNonAsciiTextRoundTrips;
    procedure TestAppendCreatesOneObject;
    procedure TestDeleteDeletesTheObject;
    procedure TestLocateFindsByFieldValue;
    procedure TestLocateByPartialKey;
    procedure TestBookmarkReturnsToTheRecord;
    procedure TestEmptyListOpensEmpty;
    procedure TestPersistentFieldsAreKept;
    procedure TestAppendRecordCreatesOneObject;
    procedure TestDateAndTimeFieldsRoundTrip;
    procedure TestBlobFieldReadsAndWrites;
    // BlockReadSize > 0 puts a dataset in the state dsBlockRead, in which
    // it is read from first to last without notifying its controls; the
    // LCL's lookup combo box reads its list that way
    procedure TestFieldsReadInBlockReadMode;
    // A Bold-side change refreshes the dataset through BoldSharedClock, which
    // needs only CheckSynchronize, not a VCL message loop (#117 step 9)
    procedure TestBoldSideChangesShowWithoutAMessageLoop;
    // Only an attribute changes: no row is added, removed or moved, and the
    // dataset has been quiet for longer than its debounce interval
    procedure TestAttributeChangeAloneShows;
    // Twice: the first change must not use up the subscription
    procedure TestSecondAttributeChangeShowsToo;
    // The dataset follows only the rows it has read; a row it has not read
    // yet must still show its current value when the cursor gets there
    procedure TestAttributeChangeOnAnUnreadRowShowsAfterScrolling;
    procedure TestAttributeChangeShowsAfterScrollingAwayAndBack;
    procedure TestOnlyRowsReadAreFollowed;
    // While a row is read and its cell followers are created, the display
    // queue is drained (a log window that pumps messages does that) and
    // brings a change of another, followed row: the dataset must refresh
    // for it, after the row is read
    procedure TestChangeOfAnotherRowWhileARowIsRead;
    procedure TestCloseWithAPendingRefresh;
    procedure TestFreeWithAPendingRefresh;
  end;

  { Two TBoldDataSets on chained list handles, as a master-detail form has
    them: the detail handle is rooted on the master handle, and scrolling the
    master dataset moves the master handle's current element (#117 step 10,
    the FMX client shows Invoicing this way). TestModel1: Own has ChildMapped,
    whose class is abstract; the children are Child1. }
  {$IFNDEF FPC}
  [Category('Handles')]
  {$ENDIF}
  TTestBoldDataSetMasterDetail = class(TBoldTestCase)
  private
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FSystemHandle: TBoldSystemHandle;
    FMasterHandle: TBoldListHandle;
    FDetailHandle: TBoldListHandle;
    FMaster: TBoldDataSet;
    FDetail: TBoldDataSet;
    FHandleNotifications: Integer;
    function AddOwn(const AChildren: array of string): TOwn;
    function DetailValues: string;
    function ExpectedDetailValues: string;
    function WaitForDetail(ATimeoutMs: Integer): Boolean;
    procedure CountHandleNotification(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDetailFollowsTheMaster;
    procedure TestDetailFollowsANewChild;
    // A form's OnDestroy runs when its components are already marked as
    // being destroyed; what it does to the objects then (Discard, say) still
    // reaches the handles and the datasets
    procedure TestObjectSpaceChangeWhileTheFormIsBeingDestroyed;
  end;

  { A single link as a dataset field (#117): the field of an expression that
    gives an object holds a key of that object, and a dataset over the
    objects that can be linked has the same key in a field with the
    expression 'self'. That is what a lookup field and a lookup combo box
    need: KeyFields = the link field, LookupKeyFields = the 'self' field.
    TestModel1: a ChildMapped (Child1) has one Own. }
  {$IFNDEF FPC}
  [Category('Handles')]
  {$ENDIF}
  TTestBoldDataSetLinks = class(TBoldTestCase)
  private
    FSystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
    FSystemHandle: TBoldSystemHandle;
    FOwnersHandle: TBoldListHandle;
    FChildrenHandle: TBoldListHandle;
    FOwners: TBoldDataSet;
    FChildren: TBoldDataSet;
    FOwn1, FOwn2: TOwn;
    FChildOfOwn1, FChildOfOwn2, FLooseChild: TChildMapped;
    function AddChild(const AName: string; AOwn: TOwn): TChildMapped;
    function KeyOf(AOwn: TOwn): Int64;
    procedure GoToChild(AChild: TChildMapped);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLinkFieldHoldsTheKeyOfTheLinkedObject;
    procedure TestLinkFieldOfAnObjectWithoutLinkIsNull;
    procedure TestPostOfAKeySetsTheLink;
    procedure TestClearRemovesTheLink;
    procedure TestCancelOfALinkEditRestoresTheLink;
    procedure TestLocateFindsTheLinkedObjectByItsKey;
    procedure TestLookupFieldShowsAValueOfTheLinkedObject;
    procedure TestLookupFieldOfAnObjectWithoutLinkIsNull;
    // A key field named 'self' is found by the object, not by a search
    procedure TestLookupByAKeyFieldNamedSelf;
    procedure TestLookupLeavesTheCursorOfTheLookupDataSet;
    procedure TestCalculatedFieldReadsTheRowItIsCalculatedFor;
    procedure TestLinkChangedOnTheBoldSideShows;
    // The children of the selected Own, as a detail grid has them: a child
    // that gets another Own leaves that list while its row is edited
    procedure TestLinkEditThatTakesTheRowOutOfItsListIsKept;
  private
    procedure CalcNameOfChild(DataSet: TDataSet);
    procedure AddLookupFields(const ALookupKeyField: string; out ALookup: TIntegerField);
  end;

  { Editing as a grid does it (#117): edit and post, cancel, append and
    delete, in a detail dataset under a master, on objects that are saved.
    The SQLite-backed BoldTestDM, BoldTestModel: a SomeClass has a parent
    and children. The fields are written to the object when they are set,
    so Cancel has to take back exactly what the edit set. }
  {$IFNDEF FPC}
  [Category('Handles')]
  {$ENDIF}
  TTestBoldDataSetEditing = class(TBoldTestCase)
  private
    FMasterHandle: TBoldListHandle;
    FDetailHandle: TBoldListHandle;
    FMaster: TBoldDataSet;
    FDetail: TBoldDataSet;
    FParent, FOtherParent: TObject;
    FChild1, FChild2, FOtherChild: TObject;
    procedure GoToMaster(AObject: TObject);
    function ObjectCount: Integer;
    procedure GoToDetail(AObject: TObject);
    procedure Settle;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPostKeepsTheValueAndTheRow;
    procedure TestCancelOfAnEditRestoresTheValue;
    procedure TestCancelOfAnEditLeavesNothingToSave;
    procedure TestCancelKeepsWhatWasPostedBefore;
    procedure TestAppendInADetailLinksTheNewObjectToTheMaster;
    procedure TestCancelOfAnAppendLeavesNoObject;
    procedure TestCancelOfAnEditOfANewObjectKeepsTheObject;
    procedure TestDeleteInADetailDeletesTheObject;
    procedure TestAFieldThatCannotBeSetIsReadOnly;
    // A grid cell is being typed in, or a new row, and the user clicks
    // another row of the master grid: the detail grid shows the rows of that
    // master, and what was typed stays with the object it was typed for
    procedure TestMasterMovesWhileADetailRowIsEdited;
    procedure TestMasterMovesWhileADetailRowIsInserted;
    // A calculated column of the row that is typed in
    procedure TestOtherFieldsFollowWhileARowIsInserted;
    procedure TestOtherFieldsFollowWhileARowIsEdited;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldSystem,
  BoldValueSpaceInterfaces,
  BoldQueue,
  dmBoldTest,
  BoldTestModel,
  dmModel1;

type
  // Reaches the protected CellFollowerCount
  TBoldDataSetAccess = class(TBoldDataSet);
  // Reaches the protected Destroying
  TComponentAccess = class(TComponent);

const
  // A Discard of one new object makes the two list handles tell four times
  // that their value changed; this is well above that
  cNotificationLimit = 12;
  cNames: array[0..2] of string = ('one', 'two', 'three');

function ExpectedDateTime(AIndex: Integer): TDateTime;
begin
  Result := EncodeDate(2026, 11, 8 + AIndex) + EncodeTime(14, 30, 0, 0);
end;

{ TTestBoldDataSet }

procedure TTestBoldDataSet.SetUp;
var
  i: Integer;
  Obj: TClassA;
begin
  inherited;
  FDataModule := TjehodmBoldTest.Create(nil);
  for i := 0 to High(cNames) do
  begin
    Obj := TClassA.Create(FDataModule.BoldSystemHandle1.System);
    Obj.aString := cNames[i];
    Obj.aInteger := i + 1;
    Obj.aBoolean := Odd(i);
    Obj.aDateTime := ExpectedDateTime(i);
  end;
  FDataSet := TBoldDataSet.Create(nil);
  FDataSet.BoldHandle := FDataModule.BoldListHandle1;
  AddField('aString');
  AddField('aInteger');
  AddField('aBoolean');
  AddField('aDateTime');
end;

procedure TTestBoldDataSet.TearDown;
begin
  FreeAndNil(FDataSet);
  FreeAndNil(FDataModule);
  inherited;
end;

procedure TTestBoldDataSet.AddField(const AExpression: string);
var
  Description: TBoldDataSetFieldDescription;
begin
  Description := FDataSet.FieldDescriptions.Add;
  Description.BoldProperties.Expression := AExpression;
  Description.FieldName := AExpression;
end;

function TTestBoldDataSet.ListObject(AIndex: Integer): TClassA;
begin
  Result := FDataModule.BoldListHandle1.List[AIndex] as TClassA;
end;

{ Two changes in a row, each displayed as the VCL's idle loop would. The
  dataset refreshes at once on the first; the second falls into its 100 ms
  debounce interval and shows only when that is over. }
procedure TTestBoldDataSet.ChangeTwiceOnTheBoldSide(ATarget: TClassA);
begin
  ATarget.aString := 'first';
  TBoldQueueable.DisplayAll;
  ATarget.aString := 'second';
  TBoldQueueable.DisplayAll;
end;

function TTestBoldDataSet.WaitForFieldValue(const AFieldName, AExpected: string; ATimeoutMs: Integer): Boolean;
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(20);
    if FDataSet.FieldByName(AFieldName).AsString = AExpected then
      Exit(True);
  until (BoldTestTickCount - StartTicks) >= ATimeoutMs;
  Result := False;
end;

{ Runs what other threads handed to the main thread with TThread.Queue, as
  VCL and FMX do from their idle loops. }
procedure TTestBoldDataSet.ProcessQueuedCallsFor(AMilliseconds: Integer);
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  while (BoldTestTickCount - StartTicks) < AMilliseconds do
    CheckSynchronize(20);
end;

procedure TTestBoldDataSet.TestOpenShowsOneRecordPerObject;
begin
  FDataSet.Open;
  Assert.IsTrue(FDataSet.Active, 'Open should activate the dataset');
  Assert.AreEqual(3, FDataSet.RecordCount, 'one record per object in ClassA.allInstances');
  Assert.AreEqual(4, FDataSet.FieldCount, 'one field per field description');
  Assert.IsTrue(FDataSet.FieldByName('aInteger').DataType = ftInteger, 'aInteger should be an integer field');
  Assert.IsTrue(FDataSet.FieldByName('aDateTime').DataType = ftDateTime, 'aDateTime should be a date-time field');
end;

procedure TTestBoldDataSet.TestFieldsReadTheAttributes;
var
  Obj: TClassA;
begin
  FDataSet.Open;
  FDataSet.First;
  Obj := ListObject(0);
  Assert.AreEqual(Obj.aString, FDataSet.FieldByName('aString').AsString, 'aString');
  Assert.AreEqual(Obj.aInteger, FDataSet.FieldByName('aInteger').AsInteger, 'aInteger');
  Assert.AreEqual(Obj.aBoolean, FDataSet.FieldByName('aBoolean').AsBoolean, 'aBoolean');
  Assert.AreEqual(Obj.aDateTime, FDataSet.FieldByName('aDateTime').AsDateTime, 'aDateTime');
  FDataSet.Next;
  Obj := ListObject(1);
  Assert.AreEqual(Obj.aString, FDataSet.FieldByName('aString').AsString, 'aString of the second record');
  Assert.AreEqual(Obj.aBoolean, FDataSet.FieldByName('aBoolean').AsBoolean, 'aBoolean of the second record');
  Assert.AreEqual(Obj.aDateTime, FDataSet.FieldByName('aDateTime').AsDateTime, 'aDateTime of the second record');
end;

procedure TTestBoldDataSet.TestNavigationFollowsTheListOrder;
var
  Visited, Expected: string;
  i: Integer;
begin
  Expected := '';
  for i := 0 to FDataModule.BoldListHandle1.Count - 1 do
    Expected := Expected + ListObject(i).aString + ',';
  FDataSet.Open;
  Visited := '';
  FDataSet.First;
  Assert.AreEqual(1, FDataSet.RecNo, 'RecNo after First');
  while not FDataSet.Eof do
  begin
    Visited := Visited + FDataSet.FieldByName('aString').AsString + ',';
    FDataSet.Next;
  end;
  Assert.AreEqual(Expected, Visited, 'First/Next should visit the list in order');
  FDataSet.Last;
  Assert.AreEqual(3, FDataSet.RecNo, 'RecNo after Last');
  Assert.AreEqual(ListObject(2).aString, FDataSet.FieldByName('aString').AsString, 'Last');
  FDataSet.Prior;
  Assert.AreEqual(ListObject(1).aString, FDataSet.FieldByName('aString').AsString, 'Prior after Last');
end;

procedure TTestBoldDataSet.TestActiveBoldObjectFollowsTheCursor;
begin
  FDataSet.Open;
  FDataSet.First;
  Assert.IsTrue(FDataSet.ActiveBoldObject = ListObject(0), 'ActiveBoldObject on the first record');
  FDataSet.Next;
  Assert.IsTrue(FDataSet.ActiveBoldObject = ListObject(1), 'ActiveBoldObject on the second record');
end;

procedure TTestBoldDataSet.TestPostWritesToTheObject;
var
  Target: TClassA;
begin
  FDataSet.Open;
  FDataSet.First;
  Target := ListObject(0);
  FDataSet.Edit;
  FDataSet.FieldByName('aString').AsString := 'uno';
  FDataSet.FieldByName('aInteger').AsInteger := 11;
  FDataSet.Post;
  Assert.AreEqual('uno', Target.aString, 'Post should write aString to the object');
  Assert.AreEqual(11, Target.aInteger, 'Post should write aInteger to the object');
  Assert.AreEqual('uno', FDataSet.FieldByName('aString').AsString, 'the record should show the posted value');
  Assert.AreEqual(3, FDataSet.RecordCount, 'Edit and Post should not add records');
end;

procedure TTestBoldDataSet.TestNonAsciiTextRoundTrips;
const
  cText = 'Åsa Ödön € Ω';
var
  Target: TClassA;
begin
  FDataSet.Open;
  FDataSet.First;
  Target := ListObject(0);
  FDataSet.Edit;
  FDataSet.FieldByName('aString').AsString := cText;
  FDataSet.Post;
  Assert.AreEqual(cText, Target.aString, 'non-ASCII text should reach the object unchanged');
  Target.aString := cText + '!';
  FDataSet.Refresh;
  // AsWideString: in Free Pascal AsString is an AnsiString in the system code page,
  // which on Windows cannot hold the Ω
  Assert.AreEqual(cText + '!', string(FDataSet.FieldByName('aString').AsWideString), 'non-ASCII text should come back unchanged');
end;

procedure TTestBoldDataSet.TestAppendCreatesOneObject;
begin
  FDataSet.Open;
  FDataSet.Append;
  FDataSet.FieldByName('aString').AsString := 'four';
  FDataSet.Post;
  Assert.AreEqual(4, FDataModule.BoldListHandle1.Count, 'Append and Post should create exactly one ClassA');
  Assert.AreEqual(4, FDataSet.RecordCount, 'the new object should be a record');
  Assert.AreEqual('four', (FDataSet.ActiveBoldObject as TClassA).aString, 'the new record should be current, with the posted value');
end;

procedure TTestBoldDataSet.TestDeleteDeletesTheObject;
var
  DeletedName: string;
  i: Integer;
begin
  FDataSet.Open;
  FDataSet.First;
  DeletedName := FDataSet.FieldByName('aString').AsString;
  FDataSet.Delete;
  Assert.AreEqual(2, FDataModule.BoldListHandle1.Count, 'Delete should delete the object');
  Assert.AreEqual(2, FDataSet.RecordCount, 'Delete should remove the record');
  for i := 0 to FDataModule.BoldListHandle1.Count - 1 do
    Assert.AreNotEqual(DeletedName, ListObject(i).aString, 'the deleted object should be gone from the list');
end;

procedure TTestBoldDataSet.TestLocateFindsByFieldValue;
begin
  FDataSet.Open;
  Assert.IsTrue(FDataSet.Locate('aString', 'two', []), 'Locate should find two');
  Assert.AreEqual(2, FDataSet.FieldByName('aInteger').AsInteger, 'Locate should move to the record of two');
  Assert.AreEqual('two', (FDataSet.ActiveBoldObject as TClassA).aString, 'ActiveBoldObject after Locate');
  Assert.IsFalse(FDataSet.Locate('aString', 'none', []), 'Locate should not find none');
end;

procedure TTestBoldDataSet.TestLocateByPartialKey;
begin
  FDataSet.Open;
  Assert.IsTrue(FDataSet.Locate('aString', 'th', [loPartialKey]), 'th should find three');
  Assert.AreEqual(3, FDataSet.FieldByName('aInteger').AsInteger, 'Locate should move to the record of three');
  Assert.IsFalse(FDataSet.Locate('aString', 'TW', [loPartialKey]), 'a partial key is case sensitive by default');
  Assert.IsTrue(FDataSet.Locate('aString', 'TW', [loPartialKey, loCaseInsensitive]), 'TW should find two without case');
  Assert.AreEqual(2, FDataSet.FieldByName('aInteger').AsInteger, 'Locate should move to the record of two');
  Assert.IsFalse(FDataSet.Locate('aString', 'twox', [loPartialKey]), 'a key longer than the value should not match');
end;

procedure TTestBoldDataSet.TestBookmarkReturnsToTheRecord;
var
  Mark: TBookmark;
  Expected: string;
begin
  FDataSet.Open;
  FDataSet.First;
  FDataSet.Next;
  Expected := FDataSet.FieldByName('aString').AsString;
  Mark := FDataSet.Bookmark;
  FDataSet.Last;
  Assert.IsTrue(FDataSet.BookmarkValid(Mark), 'the bookmark should be valid');
  FDataSet.Bookmark := Mark;
  Assert.AreEqual(Expected, FDataSet.FieldByName('aString').AsString, 'the bookmark should return to the second record');
  Assert.AreEqual(2, FDataSet.RecNo, 'RecNo after going to the bookmark');
end;

{ A field created before Open, as the IDE's fields editor does: the dataset
  uses it instead of creating its own, and Close leaves it alone. }
procedure TTestBoldDataSet.TestPersistentFieldsAreKept;
var
  Field: TIntegerField;
begin
  Field := TIntegerField.Create(FDataSet);
  Field.FieldName := 'aInteger';
  Field.DataSet := FDataSet;
  FDataSet.Open;
  FDataSet.First;
  Assert.AreEqual(1, FDataSet.FieldCount, 'only the persistent field');
  Assert.AreEqual(ListObject(0).aInteger, Field.AsInteger, 'the persistent field should read aInteger');
  FDataSet.Close;
  FDataSet.Open;
  Assert.AreEqual(1, FDataSet.FieldCount, 'Close and Open should keep the persistent field, and only it');
  Assert.IsTrue(FDataSet.FindField('aInteger') = Field, 'the same field object after Close and Open');
end;

procedure TTestBoldDataSet.TestFieldsReadInBlockReadMode;
var
  i: Integer;
begin
  FDataSet.Open;
  FDataSet.BlockReadSize := 1;
  try
    Assert.IsTrue(FDataSet.State = dsBlockRead, 'BlockReadSize should give the state dsBlockRead');
    FDataSet.First;
    for i := 0 to High(cNames) do
    begin
      Assert.IsFalse(FDataSet.Eof, 'a record for every object');
      Assert.AreEqual(cNames[i], FDataSet.FieldByName('aString').AsString, 'aString while block reading');
      Assert.AreEqual(i + 1, FDataSet.FieldByName('aInteger').AsInteger, 'aInteger while block reading');
      FDataSet.Next;
    end;
    Assert.IsTrue(FDataSet.Eof, 'and then the end');
  finally
    FDataSet.BlockReadSize := 0;
  end;
  Assert.IsTrue(FDataSet.State = dsBrowse, 'back to browsing');
end;

procedure TTestBoldDataSet.TestEmptyListOpensEmpty;
begin
  FDataModule.BoldListHandle1.Expression := 'ClassA.allInstances->select(aInteger > 100)';
  FDataSet.Open;
  Assert.IsTrue(FDataSet.Active, 'an empty list should still open');
  Assert.IsTrue(FDataSet.IsEmpty, 'IsEmpty');
  Assert.AreEqual(0, FDataSet.RecordCount, 'RecordCount');
  Assert.IsTrue(FDataSet.Bof and FDataSet.Eof, 'Bof and Eof');
end;

procedure TTestBoldDataSet.TestAppendRecordCreatesOneObject;
var
  i, Found: Integer;
begin
  FDataSet.Open;
  // TDataSet.AddRecord calls DoOnNewRecord and then InternalAddRecord
  FDataSet.AppendRecord(['four', 4, True, ExpectedDateTime(3)]);
  Assert.AreEqual(4, FDataModule.BoldListHandle1.Count, 'AppendRecord should create exactly one ClassA');
  Assert.AreEqual(4, FDataSet.RecordCount, 'the new object should be a record');
  Found := 0;
  for i := 0 to FDataModule.BoldListHandle1.Count - 1 do
    if ListObject(i).aString = 'four' then
      Inc(Found);
  Assert.AreEqual(1, Found, 'one object should hold the appended values');
end;

procedure TTestBoldDataSet.TestDateAndTimeFieldsRoundTrip;
var
  Target: TClassA;
begin
  AddField('aDate');
  AddField('aTime');
  Target := ListObject(0);
  Target.aDate := EncodeDate(2026, 12, 24);
  Target.aTime := EncodeTime(9, 15, 30, 0);
  FDataSet.Open;
  FDataSet.First;
  Assert.AreEqual(Double(EncodeDate(2026, 12, 24)), Double(FDataSet.FieldByName('aDate').AsDateTime), 'aDate should be read');
  Assert.AreEqual(Double(EncodeTime(9, 15, 30, 0)), Double(FDataSet.FieldByName('aTime').AsDateTime), 1E-9, 'aTime should be read');
  // TDateField and TTimeField write 32-bit integers into the record buffer
  FDataSet.Edit;
  FDataSet.FieldByName('aDate').AsDateTime := EncodeDate(2027, 1, 2);
  FDataSet.FieldByName('aTime').AsDateTime := EncodeTime(17, 45, 0, 0);
  FDataSet.Post;
  Assert.AreEqual(Double(EncodeDate(2027, 1, 2)), Double(Target.aDate), 'aDate should reach the object');
  Assert.AreEqual(Double(EncodeTime(17, 45, 0, 0)), Double(Target.aTime), 1E-9, 'aTime should reach the object');
end;

procedure TTestBoldDataSet.TestBlobFieldReadsAndWrites;
var
  Target: TClassA;
begin
  AddField('aBlob');
  Target := ListObject(0);
  Target.aBlob := 'blob one';
  FDataSet.Open;
  FDataSet.First;
  Assert.AreEqual('blob one', FDataSet.FieldByName('aBlob').AsString, 'the blob should be read through CreateBlobStream');
  FDataSet.Edit;
  FDataSet.FieldByName('aBlob').AsString := 'blob two';
  FDataSet.Post;
  Assert.AreEqual('blob two', Target.aBlob, 'the blob written through CreateBlobStream should reach the object');
end;

procedure TTestBoldDataSet.TestBoldSideChangesShowWithoutAMessageLoop;
begin
  FDataSet.Open;
  FDataSet.First;
  ChangeTwiceOnTheBoldSide(ListObject(0));
  Assert.IsTrue(WaitForFieldValue('aString', 'second', 3000),
    'the second change should show after the debounce, through CheckSynchronize alone');
end;

procedure TTestBoldDataSet.TestAttributeChangeAloneShows;
begin
  FDataSet.Open;
  FDataSet.First;
  // Let everything that Open and First queued be displayed, and the
  // debounce interval of the refresh it caused pass
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling');
  ListObject(0).aString := 'changed';
  Assert.IsTrue(WaitForFieldValue('aString', 'changed', 1000),
    'a changed attribute should show in the dataset without a Resync from the application');
end;

procedure TTestBoldDataSet.TestSecondAttributeChangeShowsToo;
begin
  FDataSet.Open;
  FDataSet.First;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling');
  ListObject(0).aString := 'changed';
  Assert.IsTrue(WaitForFieldValue('aString', 'changed', 1000), 'the first change');
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling again');
  ListObject(0).aString := 'changed again';
  Assert.IsTrue(WaitForFieldValue('aString', 'changed again', 1000), 'the second change');
  ListObject(0).aInteger := 4711;
  Assert.IsTrue(WaitForFieldValue('aInteger', '4711', 1000), 'a change of another field');
end;

procedure TTestBoldDataSet.TestAttributeChangeOnAnUnreadRowShowsAfterScrolling;
begin
  FDataSet.Open;
  FDataSet.First;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling');
  ListObject(2).aString := 'changed';
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling again');
  Assert.AreEqual(cNames[0], FDataSet.FieldByName('aString').AsString, 'the first row is untouched');
  FDataSet.Last;
  Assert.AreEqual('changed', FDataSet.FieldByName('aString').AsString);
end;

procedure TTestBoldDataSet.TestAttributeChangeShowsAfterScrollingAwayAndBack;
begin
  FDataSet.Open;
  FDataSet.First;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling');
  FDataSet.Last;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling on the last row');
  // The first row was read before and is not the current one now
  ListObject(0).aString := 'changed';
  ListObject(2).aString := 'last changed';
  Assert.IsTrue(WaitForFieldValue('aString', 'last changed', 1000), 'the current row');
  FDataSet.First;
  Assert.AreEqual('changed', FDataSet.FieldByName('aString').AsString, 'the row scrolled back to');
end;

procedure TTestBoldDataSet.TestOnlyRowsReadAreFollowed;
const
  cExtraRows = 200;
var
  i, FieldCount, Buffered, AfterOpen, AfterLast: Integer;
begin
  for i := 1 to cExtraRows do
    TClassA.Create(FDataModule.BoldSystemHandle1.System).aString := 'extra';
  FDataSet.Open;
  FDataSet.First;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling');
  FieldCount := FDataSet.FieldCount;
  // TDataSet reads as many rows as it has buffers (one in Delphi and ten in
  // Free Pascal without a control attached), and one more when it looks ahead
  Buffered := TBoldDataSetAccess(FDataSet).BufferCount + 1;
  AfterOpen := TBoldDataSetAccess(FDataSet).CellFollowerCount;
  Assert.IsTrue(AfterOpen >= FieldCount, 'the current row is followed: ' + IntToStr(AfterOpen));
  Assert.IsTrue(AfterOpen <= Buffered * FieldCount,
    Format('only the rows read are followed: %d followers, %d rows buffered, %d rows in all',
    [AfterOpen, Buffered, FDataSet.RecordCount]));
  FDataSet.Last;
  Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling at the end');
  AfterLast := TBoldDataSetAccess(FDataSet).CellFollowerCount;
  Assert.IsTrue(AfterLast <= 2 * Buffered * FieldCount,
    Format('jumping to the end does not follow the rows between: %d followers', [AfterLast]));
  Assert.IsTrue(AfterLast > AfterOpen, 'the rows read at the end are followed too');
end;

function TTestBoldDataSet.ChangeAnotherRowWhileACellIsCreated(aFollower: TBoldFollower): string;
begin
  Result := FHookRenderer.DefaultGetAsStringAndSubscribe(aFollower, nil);
  if FHookArmed then
  begin
    FHookArmed := False;
    FHookTarget.aString := 'changed meanwhile';
    TBoldQueueable.DisplayAll;
  end;
end;

procedure TTestBoldDataSet.CountRefresh(Sender: TObject; Field: TField);
begin
  // Field = nil: the records were read again, or the cursor moved
  if Field = nil then
    Inc(FRefreshes);
end;

function TTestBoldDataSet.RefreshesWhileWaiting(AMilliseconds: Integer): Integer;
begin
  FRefreshes := 0;
  WaitForFieldValue('aString', 'never', AMilliseconds);
  Result := FRefreshes;
end;

procedure TTestBoldDataSet.TestChangeOfAnotherRowWhileARowIsRead;
const
  cExtraRows = 200;
  cStep = 50;
var
  i, FollowersBefore: Integer;
  DataSource: TDataSource;
begin
  for i := 1 to cExtraRows do
    TClassA.Create(FDataModule.BoldSystemHandle1.System).aString := 'extra';
  DataSource := TDataSource.Create(nil);
  FHookRenderer := TBoldAsStringRenderer.Create(nil);
  try
    FHookRenderer.OnGetAsString := ChangeAnotherRowWhileACellIsCreated;
    // aInteger, the second field
    FDataSet.FieldDescriptions[1].BoldProperties.Renderer := FHookRenderer;
    DataSource.DataSet := FDataSet;
    DataSource.OnDataChange := CountRefresh;
    FDataSet.Open;
    FDataSet.First;
    FDataSet.Last;
    Assert.IsFalse(WaitForFieldValue('aString', 'never', 400), 'settling at the end');

    // The control: reading rows that were not read before causes no refresh
    FDataSet.MoveBy(-cStep);
    Assert.AreEqual(0, RefreshesWhileWaiting(400), 'refreshes after moving to rows not read before');

    FollowersBefore := TBoldDataSetAccess(FDataSet).CellFollowerCount;
    // The row the cursor is on now was read and is followed
    FHookTarget := FDataSet.ActiveBoldObject as TClassA;
    FHookArmed := True;
    FDataSet.MoveBy(-cStep);
    Assert.IsFalse(FHookArmed, 'a cell follower should have been created while moving');
    Assert.IsTrue(TBoldDataSetAccess(FDataSet).CellFollowerCount > FollowersBefore, 'rows not read before');
    Assert.IsTrue(RefreshesWhileWaiting(400) > 0,
      'the change of the other row, displayed while a row was read, should refresh the dataset');
    FDataSet.MoveBy(cStep);
    Assert.AreEqual('changed meanwhile', FDataSet.FieldByName('aString').AsString, 'back on the changed row');
  finally
    FDataSet.Close;
    FDataSet.FieldDescriptions[1].BoldProperties.Renderer := nil;
    FreeAndNil(FHookRenderer);
    DataSource.Free;
  end;
end;

procedure TTestBoldDataSet.TestCloseWithAPendingRefresh;
begin
  FDataSet.Open;
  FDataSet.First;
  ChangeTwiceOnTheBoldSide(ListObject(0));
  FDataSet.Close;
  // The pending refresh comes due on a closed dataset and must do nothing.
  // Delphi's TDataSet.Resync already does nothing there (it checks
  // IsCursorOpen); Free Pascal's does not, so under Free Pascal this guards
  // the Active check in DebouncedResyncDue.
  ProcessQueuedCallsFor(300);
  Assert.IsFalse(FDataSet.Active, 'the dataset should stay closed');
end;

procedure TTestBoldDataSet.TestFreeWithAPendingRefresh;
begin
  FDataSet.Open;
  FDataSet.First;
  ChangeTwiceOnTheBoldSide(ListObject(0));
  // The pending refresh must not reach the freed dataset (check_memory and
  // heaptrc see it if it does)
  FreeAndNil(FDataSet);
  ProcessQueuedCallsFor(300);
  Assert.IsTrue(FDataSet = nil, 'the dataset should be gone');
end;

{ TTestBoldDataSetMasterDetail }

procedure TTestBoldDataSetMasterDetail.SetUp;
var
  Description: TBoldDataSetFieldDescription;
begin
  inherited;
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;

  FMasterHandle := TBoldListHandle.Create(nil);
  FMasterHandle.RootHandle := FSystemHandle;
  FMasterHandle.StaticSystemHandle := FSystemHandle;
  FMasterHandle.Expression := 'Own.allInstances';
  FDetailHandle := TBoldListHandle.Create(nil);
  FDetailHandle.RootHandle := FMasterHandle;
  FDetailHandle.StaticSystemHandle := FSystemHandle;
  // The role's OCL name; 'ChildMapped' would be the class
  FDetailHandle.Expression := 'childMapped';

  FMaster := TBoldDataSet.Create(nil);
  FMaster.BoldHandle := FMasterHandle;
  Description := FMaster.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'childMapped->size';
  Description.FieldName := 'ChildCount';
  FDetail := TBoldDataSet.Create(nil);
  FDetail.BoldHandle := FDetailHandle;
  Description := FDetail.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'c';
  Description.FieldName := 'c';
end;

procedure TTestBoldDataSetMasterDetail.TearDown;
begin
  FreeAndNil(FDetail);
  FreeAndNil(FMaster);
  FreeAndNil(FDetailHandle);
  FreeAndNil(FMasterHandle);
  if Assigned(FSystemHandle) and FSystemHandle.Active then
  begin
    FSystemHandle.System.Discard;
    FSystemHandle.Active := False;
  end;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
  inherited;
end;

function TTestBoldDataSetMasterDetail.AddOwn(const AChildren: array of string): TOwn;
var
  i: Integer;
  Child: TChildMapped;
begin
  Result := TOwn.Create(FSystemHandle.System);
  for i := 0 to High(AChildren) do
  begin
    Child := TChild1.Create(FSystemHandle.System);
    Child.c := AChildren[i];
    Child.Own := Result;
  end;
end;

{ The detail records' c values, sorted and comma separated }
function TTestBoldDataSetMasterDetail.DetailValues: string;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    FDetail.First;
    while not FDetail.Eof do
    begin
      Values.Add(FDetail.FieldByName('c').AsString);
      FDetail.Next;
    end;
    Values.Sort;
    Result := Values.CommaText;
  finally
    Values.Free;
  end;
end;

{ The c values of the master's current Own, sorted and comma separated }
function TTestBoldDataSetMasterDetail.ExpectedDetailValues: string;
var
  Values: TStringList;
  Own: TOwn;
  i: Integer;
begin
  Values := TStringList.Create;
  try
    Own := FMaster.ActiveBoldObject as TOwn;
    for i := 0 to Own.ChildMapped.Count - 1 do
      Values.Add(Own.ChildMapped[i].c);
    Values.Sort;
    Result := Values.CommaText;
  finally
    Values.Free;
  end;
end;

{ Drains the display queue and runs what TThread.Queue handed to the main
  thread, as the VCL's and FMX's idle loops do, until the detail dataset shows
  the master's current Own }
function TTestBoldDataSetMasterDetail.WaitForDetail(ATimeoutMs: Integer): Boolean;
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(20);
    if DetailValues = ExpectedDetailValues then
      Exit(True);
  until (BoldTestTickCount - StartTicks) >= ATimeoutMs;
  Result := False;
end;

procedure TTestBoldDataSetMasterDetail.TestDetailFollowsTheMaster;
begin
  AddOwn(['a1', 'a2']);
  AddOwn(['b1']);
  FMaster.Open;
  FDetail.Open;
  FMaster.First;
  Assert.IsTrue(WaitForDetail(3000), 'the detail should show the first Own''s children, got ' + DetailValues);
  Assert.AreEqual(FMaster.FieldByName('ChildCount').AsInteger, FDetail.RecordCount, 'the detail count on the first Own');
  FMaster.Next;
  Assert.IsTrue(WaitForDetail(3000), 'the detail should follow to the second Own, got ' + DetailValues);
  Assert.AreEqual(FMaster.FieldByName('ChildCount').AsInteger, FDetail.RecordCount, 'the detail count on the second Own');
  FMaster.First;
  Assert.IsTrue(WaitForDetail(3000), 'the detail should follow back to the first Own, got ' + DetailValues);
end;

procedure TTestBoldDataSetMasterDetail.TestDetailFollowsANewChild;
var
  Own: TOwn;
  Child: TChildMapped;
begin
  Own := AddOwn(['a1']);
  FMaster.Open;
  FDetail.Open;
  FMaster.First;
  Assert.IsTrue(WaitForDetail(3000), 'the detail should show a1, got ' + DetailValues);
  // A child added on the Bold side, as another form or code would do
  Child := TChild1.Create(FSystemHandle.System);
  Child.c := 'a2';
  Child.Own := Own;
  Assert.IsTrue(WaitForDetail(3000), 'the detail should show the new child, got ' + DetailValues);
  Assert.AreEqual(2, FDetail.RecordCount, 'two children');
end;

procedure TTestBoldDataSetMasterDetail.CountHandleNotification(Sender: TObject);
begin
  Inc(FHandleNotifications);
  // Without a limit the notifications would go on until the stack overflows,
  // which takes the test runner down with it. Taking the datasets off their
  // handles ends it: they are what answers each notification by asking the
  // handle for its list again.
  if FHandleNotifications = cNotificationLimit then
  begin
    FMaster.BoldHandle := nil;
    FDetail.BoldHandle := nil;
  end;
end;

procedure TTestBoldDataSetMasterDetail.TestObjectSpaceChangeWhileTheFormIsBeingDestroyed;
var
  Failure: string;
begin
  AddOwn(['a1']);
  FMaster.Open;
  FMaster.First;
  Assert.AreEqual(1, FMaster.RecordCount, 'one Own before');

  // What TCustomForm.BeforeDestruction does to the form's components before
  // it calls OnDestroy (the VCL, FMX and the LCL alike)
  TComponentAccess(FMasterHandle).Destroying;
  TComponentAccess(FDetailHandle).Destroying;
  TComponentAccess(FMaster).Destroying;
  TComponentAccess(FDetail).Destroying;
  FHandleNotifications := 0;
  FMasterHandle.OnBeginValueIdentityChanged := CountHandleNotification;
  FDetailHandle.OnBeginValueIdentityChanged := CountHandleNotification;
  Failure := '';
  try
    try
      // The Own is new, so this deletes it: Own.allInstances changes
      FSystemHandle.System.Discard;
    except
      on E: Exception do
        Failure := E.ClassName + ': ' + E.Message;
    end;
  finally
    FMasterHandle.OnBeginValueIdentityChanged := nil;
    FDetailHandle.OnBeginValueIdentityChanged := nil;
  end;
  Assert.IsTrue(FHandleNotifications < cNotificationLimit,
    'the list handles should not keep telling that their value changed; stopped after ' +
    IntToStr(FHandleNotifications) + ' times, Discard then raised ' + Failure);
  Assert.AreEqual('', Failure, 'Discard should not raise');
  Assert.IsFalse(FSystemHandle.System.BoldDirty, 'nothing should be left to save');
end;

{ TTestBoldDataSetLinks }

procedure TTestBoldDataSetLinks.SetUp;
var
  Description: TBoldDataSetFieldDescription;
begin
  inherited;
  Ensuredm_Model;
  FSystemTypeInfoHandle := TBoldSystemTypeInfoHandle.Create(nil);
  FSystemTypeInfoHandle.BoldModel := dm_Model1.BoldModel1;
  FSystemHandle := TBoldSystemHandle.Create(nil);
  FSystemHandle.SystemTypeInfoHandle := FSystemTypeInfoHandle;
  FSystemHandle.Active := True;

  FOwn1 := TOwn.Create(FSystemHandle.System);
  FOwn2 := TOwn.Create(FSystemHandle.System);
  FChildOfOwn1 := AddChild('first', FOwn1);
  FChildOfOwn2 := AddChild('second', FOwn2);
  AddChild('third', FOwn2);
  FLooseChild := AddChild('loose', nil);

  FOwnersHandle := TBoldListHandle.Create(nil);
  FOwnersHandle.RootHandle := FSystemHandle;
  FOwnersHandle.StaticSystemHandle := FSystemHandle;
  FOwnersHandle.Expression := 'Own.allInstances';
  FChildrenHandle := TBoldListHandle.Create(nil);
  FChildrenHandle.RootHandle := FSystemHandle;
  FChildrenHandle.StaticSystemHandle := FSystemHandle;
  FChildrenHandle.Expression := 'ChildMapped.allInstances';

  FOwners := TBoldDataSet.Create(nil);
  FOwners.BoldHandle := FOwnersHandle;
  Description := FOwners.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'self';
  Description.FieldName := 'Key';
  Description := FOwners.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'childMapped->size';
  Description.FieldName := 'ChildCount';

  FChildren := TBoldDataSet.Create(nil);
  FChildren.BoldHandle := FChildrenHandle;
  Description := FChildren.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'c';
  Description.FieldName := 'c';
  Description := FChildren.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'own';
  Description.FieldName := 'Own';
end;

procedure TTestBoldDataSetLinks.TearDown;
begin
  FreeAndNil(FChildren);
  FreeAndNil(FOwners);
  FreeAndNil(FChildrenHandle);
  FreeAndNil(FOwnersHandle);
  if Assigned(FSystemHandle) and FSystemHandle.Active then
  begin
    FSystemHandle.System.Discard;
    FSystemHandle.Active := False;
  end;
  FreeAndNil(FSystemHandle);
  FreeAndNil(FSystemTypeInfoHandle);
  inherited;
end;

function TTestBoldDataSetLinks.AddChild(const AName: string; AOwn: TOwn): TChildMapped;
begin
  Result := TChild1.Create(FSystemHandle.System);
  Result.c := AName;
  Result.Own := AOwn;
end;

{ The key of an Own, as its row in the owners dataset has it }
function TTestBoldDataSetLinks.KeyOf(AOwn: TOwn): Int64;
begin
  if not FOwners.Active then
    FOwners.Open;
  FOwners.First;
  while not FOwners.Eof and (FOwners.ActiveBoldObject <> AOwn) do
    FOwners.Next;
  Assert.IsFalse(FOwners.Eof, 'the owners dataset should have a row for the object');
  Result := FOwners.FieldByName('Key').AsLargeInt;
end;

procedure TTestBoldDataSetLinks.GoToChild(AChild: TChildMapped);
begin
  if not FChildren.Active then
    FChildren.Open;
  FChildren.First;
  while not FChildren.Eof and (FChildren.ActiveBoldObject <> AChild) do
    FChildren.Next;
  Assert.IsFalse(FChildren.Eof, 'the children dataset should have a row for the object');
end;

procedure TTestBoldDataSetLinks.TestLinkFieldHoldsTheKeyOfTheLinkedObject;
begin
  FChildren.Open;
  Assert.IsTrue(FChildren.FieldByName('Own').DataType = ftLargeint, 'a link is a ftLargeint field');
  FOwners.Open;
  Assert.IsTrue(FOwners.FieldByName('Key').DataType = ftLargeint, 'self is a ftLargeint field');
  Assert.IsTrue(KeyOf(FOwn1) <> 0, 'an object has a key');
  Assert.IsTrue(KeyOf(FOwn1) <> KeyOf(FOwn2), 'two objects have two keys');
  GoToChild(FChildOfOwn1);
  Assert.IsTrue(FChildren.FieldByName('Own').AsLargeInt = KeyOf(FOwn1), 'the link field of the first child is the key of its Own');
  GoToChild(FChildOfOwn2);
  Assert.IsTrue(FChildren.FieldByName('Own').AsLargeInt = KeyOf(FOwn2), 'the link field of the second child is the key of its Own');
end;

procedure TTestBoldDataSetLinks.TestLinkFieldOfAnObjectWithoutLinkIsNull;
begin
  GoToChild(FLooseChild);
  Assert.IsTrue(FChildren.FieldByName('Own').IsNull, 'a link to nothing is a null field');
  GoToChild(FChildOfOwn1);
  Assert.IsFalse(FChildren.FieldByName('Own').IsNull, 'a link to an object is not null');
end;

procedure TTestBoldDataSetLinks.TestPostOfAKeySetsTheLink;
var
  Key2: Int64;
begin
  Key2 := KeyOf(FOwn2);
  GoToChild(FChildOfOwn1);
  FChildren.Edit;
  FChildren.FieldByName('Own').AsLargeInt := Key2;
  FChildren.Post;
  Assert.IsTrue(FChildOfOwn1.Own = FOwn2, 'Post of the key of another Own should link the child to it');
  GoToChild(FLooseChild);
  FChildren.Edit;
  FChildren.FieldByName('Own').AsLargeInt := Key2;
  FChildren.Post;
  Assert.IsTrue(FLooseChild.Own = FOwn2, 'and link a child that had no Own');
end;

procedure TTestBoldDataSetLinks.TestClearRemovesTheLink;
begin
  GoToChild(FChildOfOwn1);
  FChildren.Edit;
  FChildren.FieldByName('Own').Clear;
  FChildren.Post;
  Assert.IsTrue(FChildOfOwn1.Own = nil, 'Post of a cleared link field should remove the link');
end;

procedure TTestBoldDataSetLinks.TestCancelOfALinkEditRestoresTheLink;
var
  Key2: Int64;
begin
  Key2 := KeyOf(FOwn2);
  GoToChild(FChildOfOwn1);
  FChildren.Edit;
  FChildren.FieldByName('Own').AsLargeInt := Key2;
  Assert.IsTrue(FChildOfOwn1.Own = FOwn2, 'the link is written when the field is set');
  FChildren.Cancel;
  Assert.IsTrue(FChildOfOwn1.Own = FOwn1, 'Cancel puts the link back');
  Assert.IsTrue(FChildOfOwn1.BoldExistenceState = besExisting, 'and the object is still there');
  GoToChild(FLooseChild);
  FChildren.Edit;
  FChildren.FieldByName('Own').AsLargeInt := Key2;
  FChildren.Cancel;
  Assert.IsTrue(FLooseChild.Own = nil, 'Cancel puts back a link to nothing too');
end;

procedure TTestBoldDataSetLinks.TestLocateFindsTheLinkedObjectByItsKey;
begin
  GoToChild(FChildOfOwn2);
  FOwners.Open;
  FOwners.First;
  Assert.IsTrue(FOwners.Locate('Key', FChildren.FieldByName('Own').Value, []), 'Locate by the key of the link');
  Assert.IsTrue(FOwners.ActiveBoldObject = FOwn2, 'Locate should find the linked object');
end;

procedure TTestBoldDataSetLinks.TestLookupFieldShowsAValueOfTheLinkedObject;
var
  Lookup: TIntegerField;
begin
  AddLookupFields('Key', Lookup);
  FOwners.Open;
  GoToChild(FChildOfOwn1);
  Assert.AreEqual(1, Lookup.AsInteger, 'the Own of the first child has one child');
  GoToChild(FChildOfOwn2);
  Assert.AreEqual(2, Lookup.AsInteger, 'the Own of the second child has two children');
end;

procedure TTestBoldDataSetLinks.TestLookupFieldOfAnObjectWithoutLinkIsNull;
var
  Lookup: TIntegerField;
begin
  AddLookupFields('Key', Lookup);
  FOwners.Open;
  GoToChild(FLooseChild);
  Assert.IsTrue(Lookup.IsNull, 'a child without Own has no lookup value');
end;

procedure TTestBoldDataSetLinks.AddLookupFields(const ALookupKeyField: string; out ALookup: TIntegerField);
var
  Link: TLargeintField;
begin
  Link := TLargeintField.Create(FChildren);
  Link.FieldName := 'Own';
  Link.DataSet := FChildren;
  ALookup := TIntegerField.Create(FChildren);
  ALookup.FieldName := 'OwnChildCount';
  ALookup.FieldKind := fkLookup;
  ALookup.KeyFields := 'Own';
  ALookup.LookupDataSet := FOwners;
  ALookup.LookupKeyFields := ALookupKeyField;
  ALookup.LookupResultField := 'ChildCount';
  ALookup.DataSet := FChildren;
end;

procedure TTestBoldDataSetLinks.TestLookupByAKeyFieldNamedSelf;
var
  Lookup: TIntegerField;
begin
  FOwners.FieldDescriptions[0].FieldName := 'self';
  AddLookupFields('self', Lookup);
  FOwners.Open;
  GoToChild(FChildOfOwn1);
  Assert.AreEqual(1, Lookup.AsInteger, 'the Own of the first child has one child');
  GoToChild(FChildOfOwn2);
  Assert.AreEqual(2, Lookup.AsInteger, 'the Own of the second child has two children');
  GoToChild(FLooseChild);
  Assert.IsTrue(Lookup.IsNull, 'a child without Own has no lookup value');
end;

procedure TTestBoldDataSetLinks.TestLookupLeavesTheCursorOfTheLookupDataSet;
var
  Lookup: TIntegerField;
  First: TObject;
begin
  AddLookupFields('Key', Lookup);
  FOwners.Open;
  FOwners.First;
  First := FOwners.ActiveBoldObject;
  GoToChild(FChildOfOwn1);
  GoToChild(FChildOfOwn2);
  GoToChild(FLooseChild);
  Assert.IsTrue(FOwners.ActiveBoldObject = First, 'the lookup dataset stays on its row');
  FOwners.Next;
  Assert.IsFalse(FOwners.Eof, 'and Next goes to its second row');
  Assert.IsTrue(FOwners.ActiveBoldObject <> First, 'which is another object');
  FOwners.Next;
  Assert.IsTrue(FOwners.Eof, 'and then to the end: two rows');
end;

procedure TTestBoldDataSetLinks.CalcNameOfChild(DataSet: TDataSet);
begin
  DataSet.FieldByName('Shout').AsString := UpperCase(DataSet.FieldByName('c').AsString);
end;

procedure TTestBoldDataSetLinks.TestCalculatedFieldReadsTheRowItIsCalculatedFor;
var
  Name, Shout: TWideStringField;
begin
  Name := TWideStringField.Create(FChildren);
  Name.FieldName := 'c';
  Name.Size := 255;
  Name.DataSet := FChildren;
  Shout := TWideStringField.Create(FChildren);
  Shout.FieldName := 'Shout';
  Shout.FieldKind := fkCalculated;
  Shout.Size := 255;
  Shout.DataSet := FChildren;
  FChildren.OnCalcFields := CalcNameOfChild;
  GoToChild(FChildOfOwn1);
  Assert.AreEqual('FIRST', Shout.AsString, 'the calculated field of the first child');
  GoToChild(FLooseChild);
  Assert.AreEqual('LOOSE', Shout.AsString, 'the calculated field of the last child');
end;

procedure TTestBoldDataSetLinks.TestLinkEditThatTakesTheRowOutOfItsListIsKept;
var
  Key1: Int64;
  DetailHandle: TBoldListHandle;
  Detail: TBoldDataSet;
  Description: TBoldDataSetFieldDescription;
  StartTicks: Int64;
begin
  Key1 := KeyOf(FOwn1);
  // The owners dataset on Own2, whose children the detail dataset shows
  FOwners.First;
  while not FOwners.Eof and (FOwners.ActiveBoldObject <> FOwn2) do
    FOwners.Next;
  DetailHandle := TBoldListHandle.Create(nil);
  Detail := TBoldDataSet.Create(nil);
  try
    DetailHandle.RootHandle := FOwnersHandle;
    DetailHandle.StaticSystemHandle := FSystemHandle;
    DetailHandle.Expression := 'childMapped';
    Detail.BoldHandle := DetailHandle;
    Description := Detail.FieldDescriptions.Add;
    Description.BoldProperties.Expression := 'own';
    Description.FieldName := 'Own';
    Detail.Open;
    Assert.AreEqual(2, Detail.RecordCount, 'Own2 has two children');
    Detail.First;
    while not Detail.Eof and (Detail.ActiveBoldObject <> FChildOfOwn2) do
      Detail.Next;
    Assert.IsTrue(Detail.ActiveBoldObject = FChildOfOwn2, 'on the second child');

    Detail.Edit;
    Detail.FieldByName('Own').AsLargeInt := Key1;
    // The child is now a child of Own1: the list of the detail handle loses it
    StartTicks := BoldTestTickCount;
    repeat
      TBoldQueueable.DisplayAll;
      CheckSynchronize(20);
    until not (Detail.State in dsEditModes) or (BoldTestTickCount - StartTicks > 2000);

    Assert.IsTrue(FChildOfOwn2.BoldExistenceState = besExisting, 'the child still exists');
    Assert.IsTrue(FChildOfOwn2.Own = FOwn1, 'and keeps the Own that was picked for it');
    Assert.IsFalse(Detail.State in dsEditModes, 'the dataset is not editing a row it no longer has');
    Assert.AreEqual(1, Detail.RecordCount, 'Own2 has one child left');
    Assert.IsTrue(Detail.ActiveBoldObject <> FChildOfOwn2, 'and the cursor is on it');
  finally
    Detail.Free;
    DetailHandle.Free;
  end;
end;

procedure TTestBoldDataSetLinks.TestLinkChangedOnTheBoldSideShows;
var
  Key2: Int64;
  StartTicks: Int64;
begin
  Key2 := KeyOf(FOwn2);
  GoToChild(FChildOfOwn1);
  FChildOfOwn1.Own := FOwn2;
  StartTicks := BoldTestTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(20);
  until (FChildren.FieldByName('Own').AsLargeInt = Key2) or (BoldTestTickCount - StartTicks > 2000);
  Assert.IsTrue(FChildren.ActiveBoldObject = FChildOfOwn1, 'the cursor stays on the child');
  Assert.IsTrue(FChildren.FieldByName('Own').AsLargeInt = Key2, 'the link field should show the new Own');
end;

{ TTestBoldDataSetEditing }

procedure TTestBoldDataSetEditing.SetUp;
var
  Sys: TBoldSystem;
  Parent, Child: TSomeClass;
  Description: TBoldDataSetFieldDescription;
begin
  inherited;
  EnsureBoldTestDM;
  Sys := BoldTestDM.BoldSystemHandle1.System;
  Parent := TSomeClass.Create(Sys);
  Parent.aString := 'the parent';
  Child := TSomeClass.Create(Sys);
  Child.aString := 'first';
  Child.parent := Parent;
  FChild1 := Child;
  Child := TSomeClass.Create(Sys);
  Child.aString := 'second';
  Child.parent := Parent;
  FChild2 := Child;
  FParent := Parent;
  Parent := TSomeClass.Create(Sys);
  Parent.aString := 'the other parent';
  Child := TSomeClass.Create(Sys);
  Child.aString := 'other child';
  Child.parent := Parent;
  FOtherParent := Parent;
  FOtherChild := Child;
  // Saved: what a Cancel goes back to is in the database
  Sys.UpdateDatabase;

  FMasterHandle := TBoldListHandle.Create(nil);
  FMasterHandle.RootHandle := BoldTestDM.BoldSystemHandle1;
  FMasterHandle.StaticSystemHandle := BoldTestDM.BoldSystemHandle1;
  // The two parents
  FMasterHandle.Expression := 'SomeClass.allInstances->select(child->notEmpty)';
  FDetailHandle := TBoldListHandle.Create(nil);
  FDetailHandle.RootHandle := FMasterHandle;
  FDetailHandle.StaticSystemHandle := BoldTestDM.BoldSystemHandle1;
  FDetailHandle.Expression := 'child';

  FMaster := TBoldDataSet.Create(nil);
  FMaster.BoldHandle := FMasterHandle;
  Description := FMaster.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'aString';
  Description.FieldName := 'aString';
  Description := FMaster.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'child->size';
  Description.FieldName := 'ChildCount';
  FDetail := TBoldDataSet.Create(nil);
  FDetail.BoldHandle := FDetailHandle;
  Description := FDetail.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'aString';
  Description.FieldName := 'aString';
  // Calculated from the attribute, as a derived column of a grid
  Description := FDetail.FieldDescriptions.Add;
  Description.BoldProperties.Expression := 'aString.length';
  Description.FieldName := 'Length';
  FMaster.Open;
  FDetail.Open;
end;

procedure TTestBoldDataSetEditing.TearDown;
begin
  FreeAndNil(FDetail);
  FreeAndNil(FMaster);
  FreeAndNil(FDetailHandle);
  FreeAndNil(FMasterHandle);
  if Assigned(BoldTestDM) and BoldTestDM.BoldSystemHandle1.Active then
    BoldTestDM.BoldSystemHandle1.System.Discard;
  CloseBoldTestDM;
  inherited;
end;

function TTestBoldDataSetEditing.ObjectCount: Integer;
begin
  Result := BoldTestDM.BoldSystemHandle1.System.EvaluateExpressionAsInteger('SomeClass.allInstances->size');
end;

procedure TTestBoldDataSetEditing.GoToMaster(AObject: TObject);
begin
  FMaster.First;
  while not FMaster.Eof and (FMaster.ActiveBoldObject <> AObject) do
    FMaster.Next;
  Assert.IsTrue(FMaster.ActiveBoldObject = AObject, 'the master dataset should have a row for the object');
end;

procedure TTestBoldDataSetEditing.GoToDetail(AObject: TObject);
begin
  FDetail.First;
  while not FDetail.Eof and (FDetail.ActiveBoldObject <> AObject) do
    FDetail.Next;
  Assert.IsTrue(FDetail.ActiveBoldObject = AObject, 'the detail dataset should have a row for the object');
end;

{ What the idle loop of an application does, for a moment }
procedure TTestBoldDataSetEditing.Settle;
var
  StartTicks: Int64;
begin
  StartTicks := BoldTestTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(20);
  until BoldTestTickCount - StartTicks > 250;
end;

procedure TTestBoldDataSetEditing.TestPostKeepsTheValueAndTheRow;
begin
  Assert.AreEqual(2, FDetail.RecordCount, 'two children');
  GoToDetail(FChild2);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'second, edited';
  FDetail.Post;
  Settle;
  Assert.AreEqual('second, edited', TSomeClass(FChild2).aString, 'Post writes to the object');
  Assert.IsTrue(FDetail.ActiveBoldObject = FChild2, 'the cursor stays on the row');
  Assert.AreEqual('second, edited', FDetail.FieldByName('aString').AsString, 'the row shows it');
  Assert.IsTrue(FDetail.State = dsBrowse, 'browsing again');
end;

procedure TTestBoldDataSetEditing.TestCancelOfAnEditRestoresTheValue;
begin
  GoToDetail(FChild1);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'typed';
  FDetail.Cancel;
  Settle;
  Assert.AreEqual('first', TSomeClass(FChild1).aString, 'Cancel takes back what the edit set');
  Assert.AreEqual('first', FDetail.FieldByName('aString').AsString, 'the row shows the value from before');
  Assert.IsTrue(FDetail.ActiveBoldObject = FChild1, 'the cursor stays on the row');
  Assert.AreEqual(2, FDetail.RecordCount, 'two children');
end;

procedure TTestBoldDataSetEditing.TestCancelOfAnEditLeavesNothingToSave;
begin
  Assert.IsFalse(BoldTestDM.BoldSystemHandle1.System.BoldDirty, 'saved, so nothing to save before the edit');
  GoToDetail(FChild1);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'typed';
  Assert.IsTrue(BoldTestDM.BoldSystemHandle1.System.BoldDirty, 'the field is written to the object when it is set');
  FDetail.Cancel;
  Settle;
  // The attribute had its saved value before the edit, so Cancel discards it
  Assert.IsFalse(BoldTestDM.BoldSystemHandle1.System.BoldDirty, 'a cancelled edit leaves nothing to save');
end;

procedure TTestBoldDataSetEditing.TestCancelKeepsWhatWasPostedBefore;
begin
  GoToDetail(FChild1);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'posted';
  FDetail.Post;
  // Not saved: the object now differs from the database
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'typed';
  FDetail.Cancel;
  Settle;
  Assert.AreEqual('posted', TSomeClass(FChild1).aString,
    'Cancel takes back the edit, not what was posted before it');
  Assert.AreEqual('posted', FDetail.FieldByName('aString').AsString, 'the row shows the posted value');
end;

procedure TTestBoldDataSetEditing.TestAppendInADetailLinksTheNewObjectToTheMaster;
var
  Added: TSomeClass;
begin
  FDetail.Append;
  FDetail.FieldByName('aString').AsString := 'third';
  FDetail.Post;
  Settle;
  Assert.AreEqual(3, TSomeClass(FParent).child.Count, 'the master has a third child');
  Assert.AreEqual(3, FDetail.RecordCount, 'and the detail dataset a third row');
  Assert.IsTrue(FDetail.ActiveBoldObject is TSomeClass, 'the new row is an object');
  Added := TSomeClass(FDetail.ActiveBoldObject);
  Assert.AreEqual('third', Added.aString, 'with the posted value');
  Assert.IsTrue(Added.parent = FParent, 'linked to the master');
  Assert.AreEqual(3, FMaster.FieldByName('ChildCount').AsInteger, 'and the master row counts it');
end;

procedure TTestBoldDataSetEditing.TestCancelOfAnAppendLeavesNoObject;
var
  Before: Integer;
begin
  Before := ObjectCount;
  FDetail.Append;
  FDetail.FieldByName('aString').AsString := 'not wanted';
  FDetail.Cancel;
  Settle;
  Assert.AreEqual(Before, ObjectCount, 'Cancel of an append leaves no object behind');
  Assert.AreEqual(2, TSomeClass(FParent).child.Count, 'the master has its two children');
  Assert.AreEqual(2, FDetail.RecordCount, 'and the detail dataset its two rows');
  Assert.IsFalse(BoldTestDM.BoldSystemHandle1.System.BoldDirty, 'nothing is left to save');
end;

procedure TTestBoldDataSetEditing.TestCancelOfAnEditOfANewObjectKeepsTheObject;
var
  Added: TObject;
begin
  FDetail.Append;
  FDetail.FieldByName('aString').AsString := 'third';
  FDetail.Post;
  Settle;
  Added := FDetail.ActiveBoldObject;
  // The new object is not saved; an edit of it is cancelled
  GoToDetail(Added);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'typed';
  FDetail.Cancel;
  Settle;
  Assert.AreEqual(3, TSomeClass(FParent).child.Count, 'the new child is still there');
  Assert.AreEqual(3, FDetail.RecordCount, 'and has its row');
  Assert.AreEqual('third', TSomeClass(Added).aString, 'with the value it was posted with');
end;

procedure TTestBoldDataSetEditing.TestDeleteInADetailDeletesTheObject;
var
  Before: Integer;
begin
  Before := ObjectCount;
  GoToDetail(FChild1);
  FDetail.Delete;
  Settle;
  Assert.AreEqual(Before - 1, ObjectCount, 'Delete deletes the object');
  Assert.AreEqual(1, TSomeClass(FParent).child.Count, 'the master has one child left');
  Assert.AreEqual(1, FDetail.RecordCount, 'and the detail dataset one row');
  Assert.IsTrue(FDetail.ActiveBoldObject = FChild2, 'the cursor is on the child that is left');
  Assert.AreEqual(1, FMaster.FieldByName('ChildCount').AsInteger, 'and the master row counts one');
end;

procedure TTestBoldDataSetEditing.TestAFieldThatCannotBeSetIsReadOnly;
var
  Count: TField;
  Raised: Boolean;
begin
  // child->size is a value that is calculated, not an attribute to set
  Count := FMaster.FieldByName('ChildCount');
  Assert.AreEqual(2, Count.AsInteger, 'two children');
  Assert.IsTrue(Count.ReadOnly, 'a grid must not offer the field for editing');
  Assert.IsFalse(FMaster.FieldByName('aString').ReadOnly, 'an attribute can be edited');
  FMaster.Edit;
  Raised := False;
  try
    Count.AsInteger := 5;
  except
    on E: EDatabaseError do
      Raised := True;
  end;
  FMaster.Cancel;
  Assert.IsTrue(Raised, 'setting it is refused');
end;

procedure TTestBoldDataSetEditing.TestMasterMovesWhileADetailRowIsEdited;
begin
  GoToMaster(FParent);
  Settle;
  GoToDetail(FChild1);
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'typed';
  // A click on the other row of the master grid
  GoToMaster(FOtherParent);
  Settle;
  Assert.IsFalse(FDetail.State in dsEditModes, 'the detail dataset is not left editing a row of another master');
  Assert.AreEqual(1, FDetail.RecordCount, 'the detail dataset has the rows of the other master');
  Assert.IsTrue(FDetail.ActiveBoldObject = FOtherChild, 'the cursor is on its child');
  Assert.AreEqual('other child', FDetail.FieldByName('aString').AsString, 'and the row shows that child');
  Assert.AreEqual('typed', TSomeClass(FChild1).aString, 'what was typed stays with the object it was typed for');
  Assert.AreEqual('other child', TSomeClass(FOtherChild).aString, 'and does not reach another object');
end;

procedure TTestBoldDataSetEditing.TestMasterMovesWhileADetailRowIsInserted;
begin
  GoToMaster(FParent);
  Settle;
  FDetail.Append;
  FDetail.FieldByName('aString').AsString := 'typed';
  // A click on the other row of the master grid
  GoToMaster(FOtherParent);
  Settle;
  Assert.IsFalse(FDetail.State in dsEditModes, 'the detail dataset is not left inserting a row of another master');
  Assert.AreEqual(1, FDetail.RecordCount, 'the detail dataset has the rows of the other master');
  Assert.IsTrue(FDetail.ActiveBoldObject = FOtherChild, 'the cursor is on its child');
  Assert.AreEqual('other child', FDetail.FieldByName('aString').AsString, 'and the row shows that child');
  Assert.AreEqual(3, TSomeClass(FParent).child.Count, 'the new row stays a child of the master it was typed for');
  Assert.AreEqual(1, TSomeClass(FOtherParent).child.Count, 'and is no child of the other');
  // Back: the new child is there, with what was typed
  GoToMaster(FParent);
  Settle;
  Assert.AreEqual(3, FDetail.RecordCount, 'three rows under the first master');
  FDetail.Last;
  Assert.AreEqual('typed', FDetail.FieldByName('aString').AsString, 'the last one with what was typed');
end;

procedure TTestBoldDataSetEditing.TestOtherFieldsFollowWhileARowIsInserted;
begin
  GoToMaster(FParent);
  Settle;
  FDetail.Append;
  FDetail.FieldByName('aString').AsString := 'abc';
  Settle;
  Assert.AreEqual(3, FDetail.FieldByName('Length').AsInteger,
    'a calculated field of the row follows what is typed in a new row, before it is posted');
  FDetail.FieldByName('aString').AsString := 'abcde';
  Assert.AreEqual(5, FDetail.FieldByName('Length').AsInteger, 'at once, with no idle time in between');
  FDetail.Cancel;
end;

procedure TTestBoldDataSetEditing.TestOtherFieldsFollowWhileARowIsEdited;
begin
  GoToMaster(FParent);
  Settle;
  GoToDetail(FChild1);
  Assert.AreEqual(5, FDetail.FieldByName('Length').AsInteger, 'first');
  FDetail.Edit;
  FDetail.FieldByName('aString').AsString := 'abc';
  Assert.AreEqual(3, FDetail.FieldByName('Length').AsInteger,
    'a calculated field of the row follows what is typed, before it is posted');
  FDetail.Cancel;
  Assert.AreEqual(5, FDetail.FieldByName('Length').AsInteger, 'and what Cancel puts back');
end;

initialization
  BoldRegisterTest(TTestBoldDataSetEditing);
  BoldRegisterTest(TTestBoldDataSet);
  BoldRegisterTest(TTestBoldDataSetMasterDetail);
  BoldRegisterTest(TTestBoldDataSetLinks);

end.
