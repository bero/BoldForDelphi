unit Test.BoldDataSet;

{ DUnitX tests for TBoldDataSet, the TDataSet over a Bold list handle (#117
  step 9). TBoldDataSet is the bridge that lets FMX (through LiveBindings) and
  Lazarus data-aware controls show Bold objects.

  Every Delphi target: without BOLD_VCL (Linux, macOS, or BOLD_NO_VCL on
  Windows) TBoldDataSet and its control packs link no VCL unit. Not in the
  Free Pascal runner yet: TBoldDataSet overrides Delphi's TDataSet API. }

interface

uses
  BoldTestFramework,
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
    // A Bold-side change refreshes the dataset through BoldSharedClock, which
    // needs only CheckSynchronize, not a VCL message loop (#117 step 9)
    procedure TestBoldSideChangesShowWithoutAMessageLoop;
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
    function AddOwn(const AChildren: array of string): TOwn;
    function DetailValues: string;
    function ExpectedDetailValues: string;
    function WaitForDetail(ATimeoutMs: Integer): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDetailFollowsTheMaster;
    procedure TestDetailFollowsANewChild;
  end;

implementation

uses
  SysUtils,
  Classes,
  DB,
  BoldQueue,
  dmModel1;

const
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
  Assert.AreEqual(cText + '!', FDataSet.FieldByName('aString').AsString, 'non-ASCII text should come back unchanged');
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

initialization
  BoldRegisterTest(TTestBoldDataSet);
  BoldRegisterTest(TTestBoldDataSetMasterDetail);

end.
