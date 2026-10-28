unit Test.BoldPersistenceController;

interface

uses
  BoldTestFramework,
  BoldPersistenceController,
  BoldCondition,
  BoldId,
  BoldUpdatePrecondition,
  BoldValueSpaceInterfaces,
  BoldDefs,
  BoldElements,
  BoldDBInterfaces;

type
  // Concrete test subclass to test base class methods
  TTestablePersistenceController = class(TBoldPersistenceController)
  public
    procedure PMExactifyIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean); override;
    procedure PMFetch(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer; BoldClientID: TBoldClientID); override;
    procedure PMFetchIDListWithCondition(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer; Condition: TBoldCondition; BoldClientID: TBoldClientID); override;
    procedure PMUpdate(ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace; Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList; var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime; BoldClientID: TBoldClientID); override;
    procedure PMTranslateToGlobalIds(ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMTranslateToLocalIds(GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    procedure PMSetReadOnlyness(ReadOnlyList, WriteableList: TBoldObjectIdList); override;
    procedure ReserveNewIds(ValueSpace: IBoldValueSpace; ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList); override;
    function CanEvaluateInPS(sOCL: string; aSystem: TBoldElement; aContext: TBoldElementTypeInfo = nil; const aVariableList: TBoldExternalVariableList = nil): Boolean; override;
  end;

  TTestBoldPersistenceController = class(TBoldTestCase)
  private
    FController: TTestablePersistenceController;
    fRaiseClockTime: TDateTime;
    fRaiseTimestamp: TBoldTimestampType;
    procedure TestRollbackTransaction_RaisesException_Raiser;
    procedure TestPMTimeForTimestamp_RaisesException_Raiser;
    procedure TestPMTimestampForTime_RaisesException_Raiser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestStartTransaction;
    procedure TestCommitTransaction;
    procedure TestRollbackTransaction_RaisesException;
    procedure TestDatabaseInterface_ReturnsNil;
    procedure TestMultilinksAreStoredInObject_ReturnsFalse;
    procedure TestPMTimeForTimestamp_RaisesException;
    procedure TestPMTimestampForTime_RaisesException;
  end;

implementation

uses
  SysUtils;

{ TTestablePersistenceController }

procedure TTestablePersistenceController.PMExactifyIds(ObjectIdList: TBoldObjectIdList;
  TranslationList: TBoldIdTranslationList; HandleNonExisting: Boolean);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMFetch(ObjectIdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; MemberIdList: TBoldMemberIdList; FetchMode: Integer;
  BoldClientID: TBoldClientID);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMFetchIDListWithCondition(
  ObjectIdList: TBoldObjectIdList; ValueSpace: IBoldValueSpace; FetchMode: Integer;
  Condition: TBoldCondition; BoldClientID: TBoldClientID);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMUpdate(ObjectIdList: TBoldObjectIdList;
  ValueSpace: IBoldValueSpace; Old_Values: IBoldValueSpace;
  Precondition: TBoldUpdatePrecondition; TranslationList: TBoldIdTranslationList;
  var TimeStamp: TBoldTimeStampType; var TimeOfLatestUpdate: TDateTime;
  BoldClientID: TBoldClientID);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMTranslateToGlobalIds(
  ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMTranslateToLocalIds(
  GlobalIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.PMSetReadOnlyness(ReadOnlyList,
  WriteableList: TBoldObjectIdList);
begin
  // Stub implementation
end;

procedure TTestablePersistenceController.ReserveNewIds(ValueSpace: IBoldValueSpace;
  ObjectIdList: TBoldObjectIdList; TranslationList: TBoldIdTranslationList);
begin
  // Stub implementation
end;

function TTestablePersistenceController.CanEvaluateInPS(sOCL: string;
  aSystem: TBoldElement; aContext: TBoldElementTypeInfo;
  const aVariableList: TBoldExternalVariableList): Boolean;
begin
  Result := False;
end;

{ TTestBoldPersistenceController }

procedure TTestBoldPersistenceController.TestRollbackTransaction_RaisesException_Raiser;
begin
  FController.RollbackTransaction;
end;

procedure TTestBoldPersistenceController.TestPMTimeForTimestamp_RaisesException_Raiser;
begin
  FController.PMTimeForTimestamp(0, fRaiseClockTime);
end;

procedure TTestBoldPersistenceController.TestPMTimestampForTime_RaisesException_Raiser;
begin
  FController.PMTimestampForTime(Now, fRaiseTimestamp);
end;

procedure TTestBoldPersistenceController.SetUp;
begin
  FController := TTestablePersistenceController.Create;
end;

procedure TTestBoldPersistenceController.TearDown;
begin
  FController.Free;
end;

procedure TTestBoldPersistenceController.TestCreate;
begin
  Assert.IsNotNull(FController);
end;

procedure TTestBoldPersistenceController.TestStartTransaction;
begin
  // Should not raise - empty implementation
  FController.StartTransaction;
  Assert.Pass;
end;

procedure TTestBoldPersistenceController.TestCommitTransaction;
begin
  // Should not raise - empty implementation
  FController.CommitTransaction;
  Assert.Pass;
end;

procedure TTestBoldPersistenceController.TestRollbackTransaction_RaisesException;
begin
  Assert.WillRaise(TestRollbackTransaction_RaisesException_Raiser, EBoldFeatureNotImplementedYet );
end;

procedure TTestBoldPersistenceController.TestDatabaseInterface_ReturnsNil;
begin
  Assert.IsNull(FController.DatabaseInterface);
end;

procedure TTestBoldPersistenceController.TestMultilinksAreStoredInObject_ReturnsFalse;
begin
  Assert.IsFalse(FController.MultilinksAreStoredInObject);
end;

procedure TTestBoldPersistenceController.TestPMTimeForTimestamp_RaisesException;
var
  ClockTime: TDateTime;
begin
  fRaiseClockTime := ClockTime;
  Assert.WillRaise(TestPMTimeForTimestamp_RaisesException_Raiser, EBoldFeatureNotImplementedYet );
end;

procedure TTestBoldPersistenceController.TestPMTimestampForTime_RaisesException;
var
  Timestamp: TBoldTimestampType;
begin
  fRaiseTimestamp := Timestamp;
  Assert.WillRaise(TestPMTimestampForTime_RaisesException_Raiser, EBoldFeatureNotImplementedYet );
end;

initialization
  BoldRegisterTest(TTestBoldPersistenceController);

end.
