unit Test.BoldRegEx;

{ BoldRegExCapture: the first group of a regular expression match, one
  implementation per compiler (BoldRegEx). The SQLite index descriptions read
  a primary key's constraint name with it. }

interface

uses
  BoldTestFramework;

type
  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldRegEx = class(TBoldTestCase)
  published
    procedure TestCaptureReturnsTheFirstGroup;
    procedure TestCaptureHonoursIgnoreCase;
    procedure TestCaptureWithoutMatchClearsCaptured;
  end;

implementation

uses
  BoldRegEx;

const
  cPrimaryKeyPattern = 'CONSTRAINT\s+"?(\w+)"?\s+PRIMARY\s+KEY';
  cCreateTable = 'CREATE TABLE T (ID INTEGER NOT NULL, CONSTRAINT "PK_T" PRIMARY KEY (ID))';

procedure TTestBoldRegEx.TestCaptureReturnsTheFirstGroup;
var
  Captured: string;
begin
  Assert.IsTrue(BoldRegExCapture(cCreateTable, cPrimaryKeyPattern, False, Captured), 'The pattern matches');
  Assert.AreEqual('PK_T', Captured, 'The first group, without the quotes around it');
end;

procedure TTestBoldRegEx.TestCaptureHonoursIgnoreCase;
const
  cLowerCase = 'create table t (id integer, constraint pk_t primary key (id))';
var
  Captured: string;
begin
  Assert.IsFalse(BoldRegExCapture(cLowerCase, cPrimaryKeyPattern, False, Captured), 'Case-sensitive');
  Assert.IsTrue(BoldRegExCapture(cLowerCase, cPrimaryKeyPattern, True, Captured), 'Case-insensitive');
  Assert.AreEqual('pk_t', Captured, 'The group as it is in the text');
end;

procedure TTestBoldRegEx.TestCaptureWithoutMatchClearsCaptured;
var
  Captured: string;
begin
  Captured := 'stale';
  Assert.IsFalse(BoldRegExCapture('CREATE TABLE T (ID INTEGER)', cPrimaryKeyPattern, True, Captured), 'No constraint');
  Assert.AreEqual('', Captured, 'Nothing captured');
end;

initialization
  BoldRegisterTest(TTestBoldRegEx);

end.
