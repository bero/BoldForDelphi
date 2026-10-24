
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractSnooper;

interface
uses
  Classes,
  BoldPersistenceControllerPassthrough,
  BoldID,
  BoldValueSpaceInterfaces,
  BoldUpdatePrecondition,
  BoldCondition,
  BoldDefs,
  BoldMeta;

type
  {forward declarations}
  TBoldAbstractSnooper = class;

  TBooleanArray = array of boolean;

  TBoldAbstractSnooper = class(TBoldPersistenceControllerPassthrough)
  private
    fMoldModel: TMoldModel;
    fEventClassFlags: TBooleanArray;
    fModelSorted: Boolean;
    FOnPropagatorFailure: TBoldNotifyEventWithErrorMessage;
    fClassesToIgnore: string;
    fArrayOfClassesToIgnore: TBooleanArray;
    fUseClassEvents: boolean;
    fUseMemberLevelOSS: boolean;
    fLinkEventTranslations: TBoldIdTranslationList;
    procedure SetClassesToIgnore(const Value: string);
  protected
    procedure GenerateNonEmbeddedStateChangedEvent(OldID, NewID: TBoldObjectId; MoldClass: TMoldClass; const NonEmbeddedLinkName: string);
    function EventMemberCount(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass): integer;
    procedure ExactifyLinkEventIds(ObjectIdList: TBoldObjectIdList; const ValueSpace, Old_Values: IBoldValueSpace);
    function ResolveEventId(Id: TBoldObjectId): TBoldObjectId;
    function ObjectIdByMemberIndex(Object_Content: IBoldObjectContents; MemberIndex: integer): TBoldObjectID;
    function MemberIsEmbeddedSingleLink(MoldMember: TMoldMember; var NonEmbeddedLink: TMoldRole): Boolean;
    function MemberIsNonEmbeddedLink(MoldMember: TMoldMember; var MemberName: string): Boolean;
    function ClassNameFromObjectID(const ABoldObjectId: TBoldObjectId): string;
    procedure NonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
    procedure EnsureDataBaseLock(const ClientID: TBoldClientID); virtual;
    procedure ReleaseDataBaseLock(const ClientID: TBoldClientID); virtual;
    procedure DoPropagatorFailure(Sender: TObject; const ErrorMessage: string);
    procedure AddClassEvents(TopsortedIndex: integer);
    procedure AddEvent(const AEvent: string); virtual; abstract;
  public
    constructor Create(MoldModel: TMoldModel); virtual;
    destructor Destroy; override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure TransmitEvents(const ClientID: TBoldClientID); virtual; abstract;
    property MoldModel: TMoldModel read fMoldModel;
    property UseClassEvents: boolean read fUseClassEvents write fUseClassEvents;
    property UseMemberLevelOSS: boolean read fUseMemberLevelOSS write fUseMemberLevelOSS;
    property OnPropagatorFailure: TBoldNotifyEventWithErrorMessage read FOnPropagatorFailure write FOnPropagatorFailure;
    property ClassesToIgnore: string read fClassesToIgnore write SetClassesToIgnore;
  end;

implementation

uses
  Sysutils,

  BoldCoreConsts,
  BoldValueInterfaces,
  BoldFreeStandingValues,
  BoldObjectSpaceExternalEvents;

constructor TBoldAbstractSnooper.Create(MoldModel: TMoldModel);
begin
  inherited Create;
  fModelSorted := false;
  fMoldModel := MoldModel;
  SetLength(fEventClassFlags, MoldModel.Classes.Count);
  SetLength(fArrayOfClassesToIgnore, MoldModel.Classes.Count);
  UseClassEvents := True; // Bold original behaviour = true
  UseMemberLevelOSS := False; // Bold original behaviour = false
end;

destructor TBoldAbstractSnooper.Destroy;
begin
  FreeAndNil(fLinkEventTranslations);
  inherited;
end;

procedure TBoldAbstractSnooper.PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            MemberIdList: TBoldMemberIdList; FetchMode: Integer;
            BoldClientID: TBoldClientID);
begin
  inherited;
end;

procedure TBoldAbstractSnooper.PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID);
begin
  inherited;
end;

procedure TBoldAbstractSnooper.PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace;
            Old_Values: IBoldValueSpace;
            Precondition: TBoldUpdatePrecondition;
            TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime;
            BoldClientID: TBoldClientID);
var
  i: integer;
  LocalOld_Values: TBoldFreeStandingValueSpace;
  LocalObjectIdList: TBoldObjectIdList;
  LocalTranslationList: TBoldIdTranslationList;
  Object_Content, NewObject_Content: IBoldObjectContents;
  ObjectId: TBoldObjectID;
  MoldClass: TMoldClass;
//  MemberValue: IBoldValue;
  TopSortedIndex: Integer;
//  sl: TStringList;
begin
  assert(assigned(MoldModel), sSnooperNoModel);
  LocalObjectIdList := ObjectIdList.Clone;
  try
    LocalOld_Values := nil;
    LocalTranslationList := nil;
    if not Assigned(Old_Values) then
    begin
      LocalOld_Values := TBoldFreeStandingValueSpace.Create;
      LocalOld_Values.GetInterface(IBoldValueSpace, Old_Values);
    end;
    if not assigned(TranslationList) then
    begin
      LocalTranslationList := TBoldIdTranslationList.Create;
      TranslationList := LocalTranslationList;
    end;

    ReserveNewIds(ValueSpace, LocalObjectIdList, TranslationList);
    ValueSpace.ApplytranslationList(TranslationList);
    LocalObjectIdList.ApplyTranslationList(TranslationList);

    // Resolve inexact embedded-link target ids to their concrete class BEFORE
    // the write: targets deleted by this very update can still be looked up.
    ExactifyLinkEventIds(LocalObjectIdList, ValueSpace, Old_Values);

    EnsureDataBaseLock(BoldClientID);
    try
      inherited PMUpdate(LocalObjectIdList, ValueSpace, Old_Values, Precondition, nil, TimeStamp, TimeOfLatestUpdate, BoldClientID);
    finally
      ReleaseDataBaseLock(BoldClientID);
    end;
    if (assigned(Precondition) and (Precondition.failed)) {or (BoldClientID = NOTVALIDCLIENTID)} then
      exit;

//    if (BoldClientID <> NOTVALIDCLIENTID) then
    for i := 0 to LocalObjectIDList.Count - 1 do
    begin
      ObjectID := LocalObjectIdList[i];
      TopSortedIndex := ObjectId.TopSortedIndex;
      MoldClass := MoldModel.Classes[TopSortedIndex];
      Object_Content := ValueSpace.ObjectContentsByObjectId[ObjectID];
      Assert(Assigned(Object_Content), Format('Object [%s] of type [%s] does not exist',
        [ObjectID.AsString,
        MoldClass.ExpandedExpressionName]));
      if Object_Content.BoldPersistenceState = bvpsModified then
        case Object_Content.BoldExistenceState of
          besExisting:
            begin
              if UseClassEvents and not fArrayOfClassesToIgnore[TopSortedIndex] then
                AddClassEvents(TopSortedIndex);
              if not fArrayOfClassesToIgnore[MoldClass.TopSortedIndex] then
                AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectCreated, MoldClass.ExpandedExpressionName, '', '', ObjectID));
            end;
          besDeleted:
            begin
              if not fArrayOfClassesToIgnore[TopSortedIndex] then
              begin
                if UseClassEvents then
                  AddClassEvents(TopSortedIndex);
                AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsObjectDeleted, MoldClass.ExpandedExpressionName, '', '', ObjectID));
              end;
            end;
        end
      else
      begin
        if not fArrayOfClassesToIgnore[TopSortedIndex] then
        begin
{
          if UseMemberLevelOss then
          begin
            sl:= TStringList.Create;
            try
              for j := 0 to Object_Content.MemberCount -1 do
              begin
                MemberValue := Object_Content.ValueByIndex[j];
                if Assigned(MemberValue) and (MemberValue.BoldPersistenceState = bvpsModified) then
                begin
                  if MoldClass.AllBoldMembers[j] is TMoldAttribute then
                    sl.Add(MoldClass.AllBoldMembers[j].ExpandedExpressionName)
                  else
                  if MoldClass.AllBoldMembers[j] is TMoldRole and TMoldRole(MoldClass.AllBoldMembers[j]).EffectiveEmbedded then
                    sl.Add(MoldClass.AllBoldMembers[j].ExpandedExpressionName)
                end;
              end;
              if sl.Count > 0 then
              begin
                AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsMemberChanged, ClassNameFromObjectID(ObjectID), sl.CommaText, '', ObjectID));
              end;
            finally
              sl.free;
            end;
          end;
}
          AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsEmbeddedStateOfObjectChanged, ClassNameFromObjectID(ObjectID), '', '', ObjectID));
        end;
      end;
    end;

    begin
      for i:= LocalObjectIdList.Count - 1 downto 0 do
      begin
        ObjectId := LocalObjectIdList[i];
        Object_Content := Old_Values.ObjectContentsbyObjectId[ObjectID];
        TopSortedIndex := ObjectId.TopSortedIndex;
        if Assigned(ObjectId) then
          NewObject_Content := ValueSpace.ObjectContentsByObjectId[ObjectId]
        else
          NewObject_Content := nil;
        if not fArrayOfClassesToIgnore[TopSortedIndex] then
          NonEmbeddedStateOfObjectChanged(Object_Content, NewObject_Content,
                  MoldModel.Classes[TopSortedIndex]);
      end;
      TransmitEvents(BoldClientID);
    end;
  finally
    FreeAndNil(fLinkEventTranslations);
    Object_Content := nil;
    NewObject_Content := nil;
    if assigned(LocalOld_Values) then
    begin
      Old_Values := nil;
      FreeAndNil(LocalOld_Values);
    end;
    FreeAndNil(LocalTranslationList);
    LocalObjectIdList.Free;
  end;
end;

procedure TBoldAbstractSnooper.GenerateNonEmbeddedStateChangedEvent(OldID, NewID: TBoldObjectId; MoldClass: TMoldClass; const NonEmbeddedLinkName: string);
begin
  // The receiver rebuilds an id from the event's class name and treats it as
  // exact, so inexact ids must be named by their concrete class, not the
  // link's declared class. Resolution does not affect IsEqual (ids compare by
  // identifier, not class).
  OldID := ResolveEventId(OldID);
  NewID := ResolveEventId(NewID);
  if (Assigned(OldID) and Assigned(NewID) and not(OldID.IsEqual[NewID])) then
  begin
    AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, ClassNameFromObjectID(OldId), NonEmbeddedLinkName, '', OldID));
    AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, ClassNameFromObjectID(NewId), NonEmbeddedLinkName, '', NewID));
  end
  else if (Assigned(OldID) and not Assigned(NewID)) then
    AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, ClassNameFromObjectID(OldId), NonEmbeddedLinkName, '', OldID))
  else if (Assigned(NewID) and not Assigned(OldID)) then
    AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsNonEmbeddedStateOfObjectChanged, ClassNameFromObjectID(NewId), NonEmbeddedLinkName, '', NewID));
end;

procedure TBoldAbstractSnooper.SetClassesToIgnore(const Value: string);
var
  sl: TStringList;
  MoldClass: TMoldClass;
  i: integer;
begin
  if fClassesToIgnore = Value then
    exit;
  for i := 0 to high(fArrayOfClassesToIgnore) do
    fArrayOfClassesToIgnore[i] := false;
  sl := TStringList.Create;
  try
    Sl.CommaText := Value;
    for i := 0 to sl.count -1 do
    begin
      MoldClass := MoldModel.Classes.ItemsByName[sl[i]];
      if not Assigned(MoldClass) then
        raise Exception.CreateFmt('Invalid class name %s', [sl[i]]);
      fArrayOfClassesToIgnore[MoldClass.TopSortedIndex] := true;
    end;
    fClassesToIgnore := Value;
  finally
    sl.free;
  end;
end;

function TBoldAbstractSnooper.ObjectIdByMemberIndex(Object_Content: IBoldObjectContents; MemberIndex: integer): TBoldObjectID;
var
  Value: IBoldValue;
  IDRef: IBoldObjectIDRef;
  IDRefPair: IBoldObjectIdrefPair;
begin
  Result := nil;
  if Assigned(Object_Content) then
  begin
    Value := Object_Content.ValueByIndex[MemberIndex];
    if Assigned(Value) then
    begin
      if (Value.QueryInterface(IBoldObjectIDRef, IDRef) = S_OK) then
        Result := IDRef.Id
      else
      if (Value.QueryInterface(IBoldObjectIdrefPair, IDRefPair) = S_OK) then
        Result := IDRefPair.Id1; // or ID2 ?
    end;
  end;
end;

function TBoldAbstractSnooper.MemberIsEmbeddedSingleLink(MoldMember: TMoldMember; var NonEmbeddedLink: TMoldRole): Boolean;
var
  MoldRole: TMoldRole;
begin
  Result := false;
  NonEmbeddedLink := nil;
  if (MoldMember is TMoldRole) then
  begin
    MoldRole := MoldMember as TMoldRole;
    Result := MoldRole.EffectiveEmbedded and MoldRole.EffectivePersistent;
    if Result then
    begin
      NonEmbeddedLink := MoldRole.OtherEnd ;
      if (NonEmbeddedLink.RoleType = rtLinkRole) then
        NonEmbeddedLink :=  NonEmbeddedLink.MainRole;
    end;
  end;
end;

function TBoldAbstractSnooper.MemberIsNonEmbeddedLink(MoldMember: TMoldMember; var MemberName: string): Boolean;
var
  MoldRole: TMoldRole;
begin
  Result := false;
  if (MoldMember is TMoldRole) then
  begin
    MoldRole := (MoldMember as TMoldRole);
    Result := not (MoldRole.EffectiveEmbedded) and (MoldRole.RoleType = rtRole);
    if Result then
      MemberName := MoldRole.ExpandedExpressionName;
  end;
end;

function TBoldAbstractSnooper.EventMemberCount(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass): integer;
begin
  Result := 0;
  if Assigned(Object_Content) then
    Result := Object_Content.MemberCount
  else if Assigned(NewObject_Content) then
    Result := NewObject_Content.MemberCount;
  // The contents can carry more member slots than the model class declares
  // (e.g. stale old values kept for an id that was re-issued to a class with
  // fewer members). Indexing AllBoldMembers past its end would raise after
  // the update has already committed: the caller then sees a failed save for
  // committed data and the whole batch of OSS events is lost.
  if Result > MoldClass.AllBoldMembers.Count then
    Result := MoldClass.AllBoldMembers.Count;
end;

procedure TBoldAbstractSnooper.ExactifyLinkEventIds(ObjectIdList: TBoldObjectIdList; const ValueSpace, Old_Values: IBoldValueSpace);
var
  i, j: integer;
  ObjectId, Id, NewId: TBoldObjectId;
  TopSortedIndex: integer;
  MoldClass: TMoldClass;
  OldContent, NewContent: IBoldObjectContents;
  NonEmbeddedLink: TMoldRole;
  ExactifyList: TBoldObjectIdList;

  procedure CollectIfInexact(AId: TBoldObjectId);
  begin
    if Assigned(AId) and not AId.TopSortedIndexExact and not ExactifyList.IdInList[AId] then
      ExactifyList.Add(AId);
  end;

begin
  ExactifyList := TBoldObjectIdList.Create;
  try
    for i := 0 to ObjectIdList.Count - 1 do
    begin
      ObjectId := ObjectIdList[i];
      TopSortedIndex := ObjectId.TopSortedIndex;
      if fArrayOfClassesToIgnore[TopSortedIndex] then
        continue;
      MoldClass := MoldModel.Classes[TopSortedIndex];
      OldContent := Old_Values.ObjectContentsByObjectId[ObjectId];
      NewContent := ValueSpace.ObjectContentsByObjectId[ObjectId];
      for j := 0 to EventMemberCount(OldContent, NewContent, MoldClass) - 1 do
        if MemberIsEmbeddedSingleLink(MoldClass.AllBoldMembers[j], NonEmbeddedLink) and
          not fArrayOfClassesToIgnore[NonEmbeddedLink.OtherEnd.MoldClass.TopSortedIndex] then
        begin
          Id := ObjectIdByMemberIndex(OldContent, j);
          NewId := ObjectIdByMemberIndex(NewContent, j);
          // only ids that GenerateNonEmbeddedStateChangedEvent will name
          if not (Assigned(Id) and Assigned(NewId) and Id.IsEqual[NewId]) then
          begin
            CollectIfInexact(Id);
            CollectIfInexact(NewId);
          end;
        end;
    end;
    if ExactifyList.Count > 0 then
    begin
      fLinkEventTranslations := TBoldIdTranslationList.Create;
      PMExactifyIds(ExactifyList, fLinkEventTranslations, false);
    end;
  finally
    ExactifyList.Free;
  end;
end;

function TBoldAbstractSnooper.ResolveEventId(Id: TBoldObjectId): TBoldObjectId;
begin
  Result := Id;
  if Assigned(fLinkEventTranslations) and Assigned(Id) and not Id.TopSortedIndexExact then
    Result := fLinkEventTranslations.TranslateToNewId[Id];
end;

procedure TBoldAbstractSnooper.NonEmbeddedStateOfObjectChanged(const Object_Content, NewObject_Content: IBoldObjectContents; MoldClass: TMoldClass);
var
  j: integer;
  Id: TBoldObjectID;
  NewId: TBoldObjectID;
  NonEmbeddedLinkName: string;
  NonEmbeddedLink: TMoldRole;
begin
  for j:= 0 to EventMemberCount(Object_Content, NewObject_Content, MoldClass) - 1 do
    if MemberIsEmbeddedSingleLink(MoldClass.AllBoldMembers[j], NonEmbeddedLink) then
    begin;
      Id := ObjectIdByMemberIndex(Object_Content, j);
      NewId := ObjectIdByMemberIndex(NewObject_Content, j);
      NonEmbeddedLinkName := NonEmbeddedLink.ExpandedExpressionName;
      if not fArrayOfClassesToIgnore[NonEmbeddedLink.OtherEnd.MoldClass.TopSortedIndex] then
        GenerateNonEmbeddedStateChangedEvent(Id, NewId, NonEmbeddedLink.MoldClass, NonEmbeddedLinkName);
    end;
end;

function TBoldAbstractSnooper.ClassNameFromObjectID(const ABoldObjectId: TBoldObjectId): string;
begin
  Result := MoldModel.Classes[ABoldObjectId.TopSortedIndex].ExpandedExpressionName;
end;

procedure TBoldAbstractSnooper.DoPropagatorFailure(Sender: TObject; const ErrorMessage: string);
begin
  if Assigned(FOnPropagatorFailure) then
    FOnPropagatorFailure(Sender, ErrorMessage);
end;

procedure TBoldAbstractSnooper.AddClassEvents(TopsortedIndex: integer);
var
  MoldClass: TMoldClass;
begin
  if fEventClassFlags[TopSortedIndex] then
    exit;
  // Emit bsClassChanged for the class AND every superclass: clients watching
  // a superclass list contain the changed subclass instances too. The flag is
  // only set together with an emitted event - marking without emitting made
  // superclass watchers miss the change and dropped a later direct superclass
  // change in the same batch via the early-exit above.
  MoldClass := MoldModel.Classes[TopSortedIndex];
  while assigned(MoldClass) do
  begin
    if not fEventClassFlags[MoldClass.TopSortedIndex] then
    begin
      AddEvent(TBoldObjectSpaceExternalEvent.EncodeExternalEvent(bsClassChanged, MoldClass.ExpandedExpressionName, '', '', nil));
      fEventClassFlags[MoldClass.TopSortedIndex] := true;
    end;
    MoldClass := MoldClass.SuperClass;
  end;
end;

procedure TBoldAbstractSnooper.EnsureDataBaseLock(const ClientID: TBoldClientID);
begin
end;

procedure TBoldAbstractSnooper.ReleaseDataBaseLock(const ClientID: TBoldClientID);
begin
end;

end.
