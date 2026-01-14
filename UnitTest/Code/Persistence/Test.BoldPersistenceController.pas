unit Test.BoldPersistenceController;

interface

uses
  DUnitX.TestFramework,
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

  [TestFixture]
  TTestBoldPersistenceController = class
  private
    FController: TTestablePersistenceController;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreate;
    [Test]
    procedure TestStartTransaction;
    [Test]
    procedure TestCommitTransaction;
    [Test]
    procedure TestRollbackTransaction_RaisesException;
    [Test]
    procedure TestDatabaseInterface_ReturnsNil;
    [Test]
    procedure TestMultilinksAreStoredInObject_ReturnsFalse;
    [Test]
    procedure TestPMTimeForTimestamp_RaisesException;
    [Test]
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

procedure TTestBoldPersistenceController.Setup;
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
  Assert.WillRaise(
    procedure
    begin
      FController.RollbackTransaction;
    end,
    EBoldFeatureNotImplementedYet
  );
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
  Assert.WillRaise(
    procedure
    begin
      FController.PMTimeForTimestamp(0, ClockTime);
    end,
    EBoldFeatureNotImplementedYet
  );
end;

procedure TTestBoldPersistenceController.TestPMTimestampForTime_RaisesException;
var
  Timestamp: TBoldTimestampType;
begin
  Assert.WillRaise(
    procedure
    begin
      FController.PMTimestampForTime(Now, Timestamp);
    end,
    EBoldFeatureNotImplementedYet
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldPersistenceController);

end.
