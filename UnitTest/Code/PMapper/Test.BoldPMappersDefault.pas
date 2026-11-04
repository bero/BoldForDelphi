unit Test.BoldPMappersDefault;

{ Tests for BoldPMappersDefault, shared between DUnitX and FPCUnit. The
  error-path fixture runs on dmUndoRedo: FireDAC under Delphi, Zeos under Free
  Pascal. The Delphi-Mocks patterns for IBoldQuery/IBoldField are in
  Test.BoldPMappersDefaultMocks (Delphi only). }

interface

uses
  BoldTestFramework,
  BoldDefs;

type
  {$IFNDEF FPC}
  [Category('PMapper')]
  {$ENDIF}
  TTestBoldPMappersDefault = class(TBoldTestCase)
  published
    procedure TestBoldMaxTimestampConstant;
  end;

  { Regression fixture for the PMCreate error-path leak (issue #64).
    Needs a real SQLite-backed system: the leak sits inside
    TBoldObjectDefaultMapper.PMCreate, which only runs against a fully built
    persistence mapper graph, so mocks cannot reach it. }
  {$IFNDEF FPC}
  [Category('PMapper')]
  {$ENDIF}
  TTestBoldPMCreateErrorPath = class(TBoldTestCase)
  private
    // Free Pascal 3.2.2 has no anonymous methods: Assert.WillRaiseAny gets this.
    procedure UpdateDatabase;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFailedInsertDoesNotLeakPerTableLists;
    procedure TestRepeatedFailedUpdateNeverReportsSilentSuccess;
  end;

implementation

uses
  SysUtils,
  Classes,
  BoldSystem,
  BoldPMappersSQL,
  BoldTestModel,
  maan_UndoRedoBase;

{ TTestBoldPMappersDefault }

procedure TTestBoldPMappersDefault.TestBoldMaxTimestampConstant;
begin
  // Verify BoldMaxTimestamp constant is defined and usable
  // NewIdFromQuery uses this to determine if a timestamped ID should be created
  Assert.AreEqual(High(Integer), BOLDMAXTIMESTAMP, 'BoldMaxTimestamp should be MaxInt');
end;

{ TTestBoldPMCreateErrorPath }

procedure TTestBoldPMCreateErrorPath.UpdateDatabase;
begin
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
end;

procedure TTestBoldPMCreateErrorPath.SetUp;
begin
  inherited;
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
end;

procedure TTestBoldPMCreateErrorPath.TearDown;
begin
  if Assigned(dmUndoRedo) then
  begin
    if dmUndoRedo.BoldSystemHandle1.Active then
    begin
      dmUndoRedo.BoldSystemHandle1.System.Discard;
      dmUndoRedo.BoldSystemHandle1.Active := False;
    end;
    // The test dropped a table behind Bold's back - free the datamodule so
    // the next EnsureDM recreates the database schema from scratch.
    FreeAndNil(dmUndoRedo);
  end;
  inherited;
end;

procedure TTestBoldPMCreateErrorPath.TestFailedInsertDoesNotLeakPerTableLists;
var
  Sys: TBoldSystem;
  ObjectMapper: TBoldObjectSQLMapper;
  i: Integer;
  FailCount: Integer;
  BytesBefore, BytesAfter: Int64;
begin
  // PMCreate builds a TBoldMemberPersistenceMapperList and a TStringList per
  // table and frees them only at the bottom of the loop body; when ExecSQL
  // raises, both leak - once per failed UpdateDatabase attempt.
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  ObjectMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper.
    ObjectPersistenceMappers[Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'].TopSortedIndex]
    as TBoldObjectSQLMapper;

  TSomeClass.Create(Sys);
  // Sabotage the schema so PMCreate's INSERT fails
  UndoRedoExecSQL('DROP TABLE ' + ObjectMapper.MainTable.SQLName);

  // Warm-up: two failing attempts get the query pool and FireDAC error
  // machinery allocated.
  Assert.WillRaiseAny(UpdateDatabase, 'UpdateDatabase must fail once the class table is gone');
  Assert.WillRaiseAny(UpdateDatabase, 'second UpdateDatabase attempt must fail too');

  // Measure attempts 3 and 4 - each failing PMCreate leaks one
  // TBoldMemberPersistenceMapperList (plus indexes) and one TStringList.
  FailCount := 0;
  BytesBefore := BoldTestAllocatedBytes;
  for i := 1 to 2 do
    try
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    except
      Inc(FailCount);  // expected - table is gone
    end;
  BytesAfter := BoldTestAllocatedBytes;

  Assert.AreEqual(2, FailCount, 'both measured UpdateDatabase attempts must reach the failing INSERT');
  Assert.IsTrue(BytesAfter - BytesBefore < 400,
    Format('2 failed PMCreate calls grew heap by %d bytes - per-table MemberPMList/SQL leak on the error path',
      [BytesAfter - BytesBefore]));
end;

procedure TTestBoldPMCreateErrorPath.TestRepeatedFailedUpdateNeverReportsSilentSuccess;
var
  Sys: TBoldSystem;
  ObjectMapper: TBoldObjectSQLMapper;
  Attempt: Integer;
  Raised: Boolean;
begin
  // When PMCreate's INSERT fails, the exception path leaves the pooled exec
  // query with a negative TStrings update count, so FireDAC stops receiving
  // SQL change notifications. After enough failed attempts UpdateDatabase
  // executes a stale/empty command, reports success and marks the dirty
  // objects clean - without writing anything. Every attempt while the table
  // is missing must keep raising and keep the system dirty.
  Sys := dmUndoRedo.BoldSystemHandle1.System;
  ObjectMapper := dmUndoRedo.BoldPersistenceHandleDB1.PersistenceControllerDefault.PersistenceMapper.
    ObjectPersistenceMappers[Sys.BoldSystemTypeInfo.ClassTypeInfoByExpressionName['SomeClass'].TopSortedIndex]
    as TBoldObjectSQLMapper;

  TSomeClass.Create(Sys);
  // Sabotage the schema so PMCreate's INSERT fails
  UndoRedoExecSQL('DROP TABLE ' + ObjectMapper.MainTable.SQLName);

  for Attempt := 1 to 6 do
  begin
    Raised := False;
    try
      dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
    except
      Raised := True;
    end;
    Assert.IsTrue(Raised,
      Format('attempt %d: UpdateDatabase must keep raising while the table is missing', [Attempt]));
    Assert.IsTrue(Sys.BoldDirty,
      Format('attempt %d: system must stay dirty after a failed UpdateDatabase', [Attempt]));
  end;
end;

initialization
  BoldRegisterTest(TTestBoldPMappersDefault);
  BoldRegisterTest(TTestBoldPMCreateErrorPath);

end.
