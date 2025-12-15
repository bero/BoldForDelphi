unit maan_UndoRedoBase;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DUnitX.TestFramework,
  TestModel1,
  BoldDefs,
  BoldSystem,
  BoldDomainElement,
  BoldAttributes,
  BoldId,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  BoldHandle,
  BoldPersistenceHandle,
  BoldPersistenceHandleDB,
  BoldSubscription,
  BoldHandles,
  BoldSystemHandle,
  BoldAbstractModel,
  BoldModel,
  BoldUndoHandler,
  UndoTestModelClasses,
  BoldFreeStandingValues,
  BoldValueInterfaces,
  BoldValueSpaceInterfaces,
  BoldElements,
  BoldSQLDatabaseConfig,
  BoldPSDescriptionsSQL,
  maan_UndoRedoTestCaseUtils,
  BoldAbstractPersistenceHandleDB,
  DB,
  BoldAbstractDatabaseAdapter,
  BoldDatabaseAdapterFireDAC,
  BoldTestDatabaseConfig,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.VCLUI.Wait
  ;

type

  TdmUndoRedo = class(TDataModule)
    BoldSystemHandle1: TBoldSystemHandle;
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle2: TBoldSystemHandle;
    BoldSystemTypeInfoHandle2: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldPersistenceHandleDB2: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    FDConnection1: TFDConnection;
    FDConnection2: TFDConnection;
    BoldDatabaseAdapterFireDAC2: TBoldDatabaseAdapterFireDAC;
  private
    { Private declarations }
  protected
  public
    { Public declarations }
  end;

  Tmaan_UndoRedoAbstractTestCase = class
  private
    FUndoHandler: TBoldUndoHandler;
    FFSValueSpace: TBoldFreeStandingValueSpace;
    FBookList, FBookList2: TBookList;
    FTopiclist, FTopicList2: TTopicList;
  protected
    FAPersistentClassList: TAPersistentClassList;
    FATransientClassList: TATransientClassList;
    procedure StoreValue(const Member: TBoldMember);
    procedure StoreObject(const Obj: TBoldObject);
    function GetStoredValueOfMember(const Member: TBoldMember): IBoldValue;
    function GetStoredObjectContents(const Obj: TBoldObject): IBoldObjectContents;
    function GetSystem: TBoldSystem;
    function GetSystem2: TBoldSystem;
    function GetUndohandler: TBoldUndoHandler;
    procedure FetchClass(const System: TBoldSystem; const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
    function IdCompare(Item1, Item2: TBoldElement): Integer;
  public
    FSomeClassList, FSomeClassList2: TSomeClassList;
    FClassWithLinkList, FClassWithLinkList2: TClassWithLinkList;
    FSubscriber: TLoggingSubscriber;
    [Setup]
    procedure SetUp; virtual;
    [TearDown]
    procedure TearDown; virtual;
    procedure RefreshSystem;
    procedure UpdateDatabase;
    procedure OpenSystem2;
    procedure SaveAndCloseSystem2;
    procedure TestClassListOrder;
    procedure SetSimpleConfiguration;
    procedure SetTransientConfiguration;
    procedure SetConfigurationForIndirectSingle;
    procedure VerifyIsInRedoArea(Member: TBoldmember; Value: TBoldFreeStandingValue);
    property System: TBoldSystem read GetSystem;
    property System2: TBoldSystem read GetSystem2;
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  end;

  procedure CloseAll;
  procedure EnsureDM;

var
  dmUndoRedo: TdmUndoRedo;

implementation

{$R maan_UndoRedo.DFM}

uses BoldUndoInterfaces;

procedure CloseAll;
begin
  if dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    dmUndoRedo.BoldSystemHandle1.Active := false;
  end;
end;

procedure EnsureDM;
var
  PersHandle: TBoldPersistenceHandleDb;
begin
  try
    if not assigned(dmUndoRedo) then
    begin
      if not Assigned(Application) then
        raise Exception.Create('Application is nil');
      Application.Initialize;

      // Create the test database first
      CreateTestDatabase;

      dmUndoRedo := TdmUndoRedo.Create(Application);
      if not Assigned(dmUndoRedo) then
        raise Exception.Create('Failed to create dmUndoRedo');

      // Configure database connections from INI file
      ConfigureConnection(dmUndoRedo.FDConnection1, dmUndoRedo.BoldDatabaseAdapterFireDAC1);
      ConfigureConnection(dmUndoRedo.FDConnection2, dmUndoRedo.BoldDatabaseAdapterFireDAC2);

      if not Assigned(dmUndoRedo.BoldSystemHandle1) then
        raise Exception.Create('BoldSystemHandle1 is nil');
      if not Assigned(dmUndoRedo.BoldSystemHandle1.PersistenceHandle) then
        raise Exception.Create('PersistenceHandle is nil');

      PersHandle := dmUndoRedo.BoldSystemHandle1.PersistenceHandle as TBoldPersistenceHandleDb;

      // Open connection and create database schema
      dmUndoRedo.FDConnection1.Open;
      if not dmUndoRedo.FDConnection1.Connected then
        raise Exception.Create('FDConnection1 failed to open');

      PersHandle.CreateDataBaseSchema();
      dmUndoRedo.BoldSystemHandle1.Active := True;

      if not Assigned(dmUndoRedo.BoldSystemHandle1.System) then
        raise Exception.Create('System is nil after activation');
    end else
    begin
      if Assigned(dmUndoRedo.BoldSystemHandle1.System) then
        dmUndoRedo.BoldSystemHandle1.System.Discard;
      dmUndoRedo.BoldSystemHandle1.Active := False;
      dmUndoRedo.BoldSystemHandle1.Active := True;
    end;
  except
    on E: Exception do
      raise Exception.Create('EnsureDM failed: ' + E.Message);
  end;
end;

{ Tmaan_UndoRedoAbstractTestCase }

function Tmaan_UndoRedoAbstractTestCase.IdCompare(Item1, Item2: TBoldElement): Integer;
var
  i1,i2: integer;
begin
  i1 := StrToInt(TBoldObject(Item1).BoldObjectLocator.AsString);
  i2 := StrToInt(TBoldObject(Item2).BoldObjectLocator.AsString);

  if  i1 = i2 then
    result := 0
  else
    if i1 < i2 then
      result := -1
    else
      result := 1;
end;

procedure Tmaan_UndoRedoAbstractTestCase.FetchClass(const System: TBoldSystem;
  const aList: TBoldObjectList; const ObjClass: TBoldObjectClass);
begin
  maan_UndoRedoTestCaseUtils.FetchClass(System, aList, ObjClass);
  aList.Sort(IdCompare);
end;

function Tmaan_UndoRedoAbstractTestCase.GetStoredObjectContents(
  const Obj: TBoldObject): IBoldObjectContents;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(obj.BoldObjectLocator.BoldObjectID);
  Result := oc as IBoldObjectContents;
end;

function Tmaan_UndoRedoAbstractTestCase.GetStoredValueOfMember(
  const Member: TBoldMember): IBoldValue;
var
  oc: TBoldFreeStandingObjectContents;
begin
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  Result := oc.ValueByIndex[Member.BoldMemberRTInfo.index];
end;

function Tmaan_UndoRedoAbstractTestCase.GetSystem: TBoldSystem;
begin
  if Assigned(dmUndoRedo) then
    Result := dmUndoRedo.BoldSystemHandle1.System
  else
    Result := nil;
end;

function Tmaan_UndoRedoAbstractTestCase.GetSystem2: TBoldSystem;
begin
  Result := dmUndoRedo.BoldSystemHandle2.System;
end;

function Tmaan_UndoRedoAbstractTestCase.GetUndohandler: TBoldUndoHandler;
begin
  Result := (System.UndoHandler as TBoldUndoHandler);
end;

procedure Tmaan_UndoRedoAbstractTestCase.OpenSystem2;
begin
  // For SQLite in-memory, both connections share the same database file
  // Copy connection params from Connection1 to Connection2
  dmUndoRedo.FDConnection2.Params.Values['Database'] := dmUndoRedo.FDConnection1.Params.Values['Database'];
  dmUndoRedo.BoldSystemHandle2.Active := true;
  FetchClass(System2, FSomeClassList2, TSomeClass);
  FetchClass(System2, FTopicList2, TTopic);
  FetchClass(System2, FBookList2, TBook);
  FetchClass(System2, FClassWithLinkList2, TClassWithLink);
end;

procedure Tmaan_UndoRedoAbstractTestCase.RefreshSystem;
begin
  UpdateDatabase;
  FSubscriber.Refresh;
  dmUndoRedo.BoldSystemHandle1.Active := false;
  FSomeClassList.Clear;
  FBookList.clear;
  fTopicList.Clear;
  FAPersistentClassList.Clear;
  FATransientClassList.Clear;
  FClassWithLinkList.Clear;
  dmUndoRedo.BoldSystemHandle1.Active := true;
  // Re-enable undo tracking after system refresh (UndoHandler is recreated with Enabled=false)
  FUndoHandler := (dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler);
  FUndoHandler.Enabled := True;
  FetchClass(System, FSomeClassList, TSomeClass);
  FetchClass(System, FBookList, TBook);
  FetchClass(System, FTopicList, TTopic);
  FetchClass(System, FClassWithLinkList, TClassWithLink);
end;

procedure Tmaan_UndoRedoAbstractTestCase.SaveAndCloseSystem2;
begin
  if Assigned(dmUndoRedo) and (dmUndoRedo.BoldSystemhandle2.Active) then
  begin
    dmUndoRedo.BoldSystemHandle2.UpdateDatabase;
    dmUndoRedo.BoldSystemHandle2.Active := false;
    FSomeClassList2.Clear;
    FBookList2.clear;
    fTopicList2.Clear;
    FClassWithLinkList2.Clear;
  end;
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetConfigurationForIndirectSingle;
begin
  GenerateObjects(System, 'ClassWithLink', 4);
  UpdateDatabase;
  FetchClass(System, FClassWithLinkList, TClassWithLink);
  FClassWithLinkList[1].one := FClassWithLinkList[0];
  FClassWithLinkList[3].one := FClassWithLinkList[2];
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetSimpleConfiguration;
begin
  GenerateObjects(System, 'SomeClass', 4);
  UpdateDatabase;
  FetchClass(System, FSomeClassList, TSomeClass);
  FSomeClassList[1].parent := FSomeClassList[0];
  FSomeClassList[3].parent := FSomeClassList[2];
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetTransientConfiguration;
begin
  RefreshSystem;
  GenerateObjects(System, 'APersistentClass', 2);
  RefreshSystem;
  FetchEnsuredClass(System, FAPersistentClassList, TAPersistentClass);
  CreateATransientClass(System, nil);
  CreateATransientClass(System, nil);
  FetchClass(System, FATransientClasslist, TATransientClass);
  FATransientClassList[0].many.Add(FAPersistentClassList[0]);
  FATransientClassList[1].many.Add(FAPersistentClassList[1]);
end;

procedure Tmaan_UndoRedoAbstractTestCase.SetUp;
begin
  inherited;
  EnsureDM;
  if not Assigned(dmUndoRedo) then
    raise Exception.Create('dmUndoRedo is nil after EnsureDM');
  if not Assigned(dmUndoRedo.BoldSystemHandle1) then
    raise Exception.Create('BoldSystemHandle1 is nil');
  if not Assigned(dmUndoRedo.BoldSystemHandle1.System) then
    raise Exception.Create('System is nil - BoldSystemHandle1.Active=' + BoolToStr(dmUndoRedo.BoldSystemHandle1.Active, True));
  if not Assigned(dmUndoRedo.BoldSystemHandle1.System.UndoHandler) then
    raise Exception.Create('UndoHandler is nil');
  FUndoHandler := (dmUndoRedo.BoldSystemHandle1.System.UndoHandler as TBoldUndoHandler);
  FUndoHandler.Enabled := True;  // Enable undo tracking for tests
  FSubscriber := TLoggingSubscriber.Create;
  FFSValueSpace := TBoldFreeStandingValueSpace.Create;
  FSomeClassList := TSomeClassList.Create;
  FSomeClassList2 := TSomeClassList.Create;
  FBookList := TBookList.Create;
  FBookList2 := TBookList.Create;
  FTopicList := TTopicList.Create;
  FTopicList2 := TTopicList.Create;
  FAPersistentClassList := TAPersistentClassList.Create;
  FATransientClassList := TATransientClassList.Create;
  FClassWithLinkList := TClassWithLinkList.Create;
  FClassWithLinkList2 := TClassWithLinkList.Create;
end;

procedure Tmaan_UndoRedoAbstractTestCase.StoreObject(
  const Obj: TBoldObject);
var
  oc: TBoldFreeStandingObjectContents;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Obj.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Obj.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Obj.AsIBoldObjectContents[bdepContents], true, false);
end;

procedure Tmaan_UndoRedoAbstractTestCase.StoreValue(
  const Member: TBoldMember);
var
  oc: TBoldFreeStandingObjectContents;
  MemberId: TBoldMemberId;
begin
  (FFSValueSpace as IBoldValueSpace).EnsureObjectContents(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc := FFSValueSpace.GetFSObjectContentsByObjectId(Member.OwningObject.BoldObjectLocator.BoldObjectID);
  oc.ApplyObjectContents(Member.OwningObject.AsIBoldObjectContents[bdepContents], false, false);
  try
    MemberId := TBoldMemberID.Create(Member.BoldMemberRTInfo.index);
    oc.EnsureMember(MemberId, member.AsIBoldValue[bdepContents].ContentName);
    oc.ValueByIndex[MemberId.MemberIndex].AssignContent(Member.AsIBoldValue[bdepContents]);
  finally
    FreeAndNil(MemberId);
  end;
end;

procedure Tmaan_UndoRedoAbstractTestCase.TearDown;
begin
  SaveAndCloseSystem2;
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemhandle1.UpdateDAtabase;
      dmUndoRedo.BoldSystemHandle1.Active := false;
    end;
    FreeAndNil(dmUndoRedo);
  end;
  FreeAndNil(FSubscriber);
  FreeAndNil(FFSValueSpace);
  FreeAndNil(FSomeClassList);
  FreeAndNil(FSomeClassList2);
  FreeAndNil(FBookList);
  FreeAndNil(FBookList2);
  FreeAndNil(FTopicList);
  FreeAndNil(FTopicList2);
  FreeAndNil(FAPersistentClassList);
  FreeAndNil(FATransientClassList);
  FreeAndNil(FClassWithLinkList);
  FreeAndNil(FClassWithLinkList2);
end;

procedure Tmaan_UndoRedoAbstractTestCase.TestClassListOrder;
var
  i: Integer;
begin
  GenerateObjects(System, 'SomeClass', 4);
  FetchClass(System, FSomeClassList, TSomeClass);
  for i := 0 to 4 - 1 do
  begin
    Assert.IsTrue(FSomeClassList[i].BoldObjectLocator.AsString = IntToStr(i), FSomeClassList[i].BoldObjectLocator.AsString + ' <> ' + IntToStr(i));
    FSomeClassList[i].aString := 'SomeClass' + IntToStr(i);
  end;
  UpdateDatabase;
  RefreshSystem;
  FetchClass(System, FSomeClassList, TSomeClass);
  Assert.IsTrue(FSomeClassList[0].aString = 'SomeClass0');
  Assert.IsTrue(FSomeClassList[1].aString = 'SomeClass1');
  Assert.IsTrue(FSomeClassList[2].aString = 'SomeClass2');
  Assert.IsTrue(FSomeClassList[3].aString = 'SomeClass3');
end;

procedure Tmaan_UndoRedoAbstractTestCase.UpdateDatabase;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure Tmaan_UndoRedoAbstractTestCase.VerifyIsInRedoArea(
  Member: TBoldmember; Value: TBoldFreeStandingValue);
var
  ValueInBlock: IBoldValue;
  res: Boolean;
begin
  ValueInBlock := nil;
  res := false;
  Assert.IsTrue(UndoHandler.RedoBlocks.CurrentBlock.ValueExists(Member.OwningObject.BoldObjectLocator.BoldObjectID,
    Member.BoldMemberRTInfo.index, ValueInBlock));
  if Member.OwningObject is TSomeClass then
    res := (Member.OwningObject as TSomeClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if Member.OwningObject is TAPersistentClass then
    res := (Member.OwningObject as TAPersistentClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if Member.OwningObject is TATransientClass then
    res := (Member.OwningObject as TATransientClass).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  else if (Member.OwningObject is TClassWithLink) then
    res := (Member.OwningObject as TClassWithLink).ValuesAreEqual(Value, ValueInBlock, Member.BoldMemberRTInfo.ExpressionName)
  ;
  Assert.IsTrue(res, Format('%s VerifyIsInRedoArea failed', [member.DisplayName]));
end;

initialization
  Randomize;
  BoldCleanDatabaseForced := True;  // Suppress confirmation dialog during automated tests

finalization
  // Clean up test database when tests are done
  try
    if Assigned(dmUndoRedo) then
    begin
      if dmUndoRedo.BoldSystemHandle1.Active then
        dmUndoRedo.BoldSystemHandle1.Active := False;
      if dmUndoRedo.FDConnection1.Connected then
        dmUndoRedo.FDConnection1.Close;
      FreeAndNil(dmUndoRedo);
    end;
    DropTestDatabase;
  except
    // Ignore errors during cleanup
  end;

end.
