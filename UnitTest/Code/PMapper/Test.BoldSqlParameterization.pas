unit Test.BoldSqlParameterization;

{ Tests that persistence-mapper SQL uses query parameters instead of literal
  ids. Literal id lists make every statement unique text, which on SQL Server
  costs a compile per statement and floods the plan cache with single-use
  ad-hoc plans. }

interface

uses
  Classes,
  SysUtils,
  DUnitX.TestFramework,
  BoldDefs,
  BoldLogHandler,
  BoldSystem,
  BoldTestModel,
  maan_UndoRedoBase;

type
  { Captures every SQL statement the DB adapter executes, by hooking the
    global BoldSQLLogHandler used by BoldLogSQLWithParams. }
  TSQLCaptureLogHandler = class(TBoldLogHandler)
  private
    fLines: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const s: string; LogType: TBoldLogType = ltInfo); override;
    procedure Clear; override;
    function CapturedText: string;
    function ContainsText(const SubText: string): Boolean;
    { All captured statements (first line + continuation/param lines) that
      contain SubText, concatenated. Empty if no statement matched. }
    function StatementsContaining(const SubText: string): string;
  end;

  [TestFixture]
  [Category('PMapper')]
  TTestBoldSqlParameterization = class
  private
    FCapture: TSQLCaptureLogHandler;
    FSavedHandler: TBoldLogHandler;
    function GetSystem: TBoldSystem;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestMultilinkFetchUsesParams;
    [Test]
    procedure TestDeleteUsesParams;
  end;

implementation

uses
  BoldDBInterfaces;

{ TSQLCaptureLogHandler }

constructor TSQLCaptureLogHandler.Create;
begin
  inherited Create;
  fLines := TStringList.Create;
end;

destructor TSQLCaptureLogHandler.Destroy;
begin
  FreeAndNil(fLines);
  inherited;
end;

procedure TSQLCaptureLogHandler.Log(const s: string; LogType: TBoldLogType);
begin
  fLines.Add(s);
end;

procedure TSQLCaptureLogHandler.Clear;
begin
  fLines.Clear;
end;

function TSQLCaptureLogHandler.CapturedText: string;
begin
  result := fLines.Text;
end;

function TSQLCaptureLogHandler.ContainsText(const SubText: string): Boolean;
begin
  result := Pos(SubText, CapturedText) > 0;
end;

function TSQLCaptureLogHandler.StatementsContaining(const SubText: string): string;
var
  i: Integer;
  Statement: string;

  procedure FlushIfMatching;
  begin
    if Pos(SubText, Statement) > 0 then
      result := result + Statement;
    Statement := '';
  end;

begin
  result := '';
  Statement := '';
  for i := 0 to fLines.Count - 1 do
  begin
    // BoldLogSQLWithParams starts each statement with a 'SQL <n>- ...' line;
    // continuation and parameter lines are indented.
    if Pos(':SQL ', fLines[i]) > 0 then
      FlushIfMatching;
    Statement := Statement + fLines[i] + sLineBreak;
  end;
  FlushIfMatching;
end;

{ TTestBoldSqlParameterization }

procedure TTestBoldSqlParameterization.SetUp;
begin
  EnsureDM;
  if not dmUndoRedo.BoldSystemHandle1.Active then
    dmUndoRedo.BoldSystemHandle1.Active := True;
  FCapture := TSQLCaptureLogHandler.Create;
  FSavedHandler := BoldSQLLogHandler;
  BoldSQLLogHandler := FCapture;
end;

procedure TTestBoldSqlParameterization.TearDown;
begin
  BoldSQLLogHandler := FSavedHandler;
  FreeAndNil(FCapture);
  if Assigned(dmUndoRedo) and dmUndoRedo.BoldSystemHandle1.Active then
  begin
    dmUndoRedo.BoldSystemHandle1.System.Discard;
    dmUndoRedo.BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(dmUndoRedo);
end;

function TTestBoldSqlParameterization.GetSystem: TBoldSystem;
begin
  result := dmUndoRedo.BoldSystemHandle1.System;
end;

procedure TTestBoldSqlParameterization.TestMultilinkFetchUsesParams;
var
  Parent, Child1, Child2: TSomeClass;
  ParentIdString: string;
begin
  Parent := TSomeClass.Create(GetSystem);
  Child1 := TSomeClass.Create(GetSystem);
  Child2 := TSomeClass.Create(GetSystem);
  Child1.parent := Parent;
  Child2.parent := Parent;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;
  ParentIdString := Parent.BoldObjectLocator.BoldObjectID.AsString;

  // Force the child multilink to be refetched from the database through
  // TBoldNonEmbeddedLinkDefaultMapper.PMFetch.
  Parent.child.Invalidate;
  FCapture.Clear;
  Assert.AreEqual(2, Parent.child.Count, 'Both children must be refetched');

  Assert.IsTrue(FCapture.ContainsText('SELECT'),
    'The invalidated multilink must be refetched with a SELECT ' +
    '(scenario precondition - no capture means the spy is not hooked up)');
  Assert.IsTrue(FCapture.ContainsText(':ID1'),
    'Multilink fetch must bind the owning ids as parameters (:ID1). ' +
    'Literal ids make every fetch unique SQL text: ' + FCapture.CapturedText);
  Assert.IsFalse(FCapture.ContainsText('= ' + ParentIdString),
    'Multilink fetch must not inline the owning id as a literal: ' +
    FCapture.CapturedText);
end;

procedure TTestBoldSqlParameterization.TestDeleteUsesParams;
var
  ObjA, ObjB: TSomeClass;
  DeleteStatements: string;
begin
  ObjA := TSomeClass.Create(GetSystem);
  ObjB := TSomeClass.Create(GetSystem);
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  ObjA.Delete;
  ObjB.Delete;
  FCapture.Clear;
  dmUndoRedo.BoldSystemHandle1.UpdateDatabase;

  // Other statements in the update flow (object fetches etc.) may already be
  // parameterized, so the assertion must look inside the DELETEs only.
  DeleteStatements := FCapture.StatementsContaining('DELETE FROM');
  Assert.IsTrue(DeleteStatements <> '',
    'Deleting saved objects must execute DELETE statements ' +
    '(scenario precondition - no capture means the spy is not hooked up)');
  Assert.IsTrue(Pos(':ID1', DeleteStatements) > 0,
    'Delete must bind the ids as parameters (:ID1). Literal id lists make ' +
    'every delete unique SQL text: ' + DeleteStatements);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestBoldSqlParameterization);

end.
