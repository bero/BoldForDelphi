{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldExceptionTrace;

{ Tests of BoldExceptionTrace (#130): the report of an exception has its
  class, its message and its call stack, and goes to Bold's log.

  The call stack is what the runtime library gives. Under Delphi that is
  Exception.StackTrace, which a provider fills; the test program links none,
  so these tests install a fake one, and also try it with none at all. Under
  Free Pascal it is the RTL's own frames; the test program is compiled with
  -gl (UnitTest/FPC/build.sh), so the frames name their routines. }

interface

uses
  SysUtils,
  BoldTestFramework;

type
  EBoldTraceTest = class(Exception);

  TTestBoldExceptionTrace = class(TBoldTestCase)
  published
    procedure TestTheReportHasTheClassAndTheMessage;
    procedure TestBoldLogExceptionWritesTheReportToTheLog;
    {$IFDEF FPC}
    // Free Pascal's own frames, the raising routine first
    procedure TestTheRaisingRoutineIsFirstInTheCallStack;
    {$ELSE}
    // A provider's text, without its capture routines and the RTL's raise
    // routines, which come before the raising routine
    procedure TestTheProvidersCallStackStartsAtTheRaisingRoutine;
    // No provider: the report says where the exception was raised
    procedure TestWithoutAProviderTheReportSaysWhereItWasRaised;
    {$ENDIF}
  end;

implementation

uses
  Classes,
  BoldLogInterfaces,
  BoldLogSinks,
  BoldExceptionTrace;

const
  cMessage = 'raised by the test';

procedure RaiseInAKnownRoutine;
begin
  raise EBoldTraceTest.Create(cMessage);
end;

function LineCount(const AText: string): Integer;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    Result := Lines.Count;
  finally
    Lines.Free;
  end;
end;

function FirstLine(const AText: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    if Lines.Count > 0 then
      Result := Lines[0]
    else
      Result := '';
  finally
    Lines.Free;
  end;
end;

{$IFNDEF FPC}
type
  TGetExceptionStackInfo = function(P: PExceptionRecord): Pointer;
  TGetStackInfoString = function(Info: Pointer): string;
  TCleanUpStackInfo = procedure(Info: Pointer);

var
  FakeInfo: Integer;

{ What a provider such as Stacker gives: its capture routines and the RTL's
  raise routines first, then the raising routine and its callers }
function FakeGetExceptionStackInfo(P: PExceptionRecord): Pointer;
begin
  Result := @FakeInfo;
end;

function FakeGetStackInfoString(Info: Pointer): string;
begin
  Result :=
    'Provider.Capture.CaptureFrames (Line 30, "Provider.Capture.pas")' + sLineBreak +
    'System.SysUtils.Exception.RaisingException' + sLineBreak +
    'System.@RaiseExcept' + sLineBreak +
    'Test.BoldExceptionTrace.RaiseInAKnownRoutine (Line 42, "Test.BoldExceptionTrace.pas")' + sLineBreak +
    'Test.BoldExceptionTrace.TheCaller (Line 50, "Test.BoldExceptionTrace.pas")';
end;

procedure FakeCleanUpStackInfo(Info: Pointer);
begin
end;
{$ENDIF}

{ TTestBoldExceptionTrace }

procedure TTestBoldExceptionTrace.TestTheReportHasTheClassAndTheMessage;
var
  Report: string;
begin
  Report := '';
  try
    RaiseInAKnownRoutine;
  except
    on E: Exception do
      Report := BoldExceptionReport(E);
  end;
  Assert.AreEqual('EBoldTraceTest: ' + cMessage, FirstLine(Report), 'the first line of the report');
  Assert.IsTrue(LineCount(Report) > 1, 'a call stack or where it was raised follows: ' + Report);
end;

procedure TTestBoldExceptionTrace.TestBoldLogExceptionWritesTheReportToTheLog;
var
  Memory: TBoldMemoryLogSink;
  Sink: IBoldLogSink;
begin
  Memory := TBoldMemoryLogSink.Create;
  Sink := Memory;
  BoldLogManager.RegisterSink(Sink);
  try
    try
      RaiseInAKnownRoutine;
    except
      on E: Exception do
        BoldLogException(E, 'while testing');
    end;
    Assert.IsTrue(Pos('while testing', Memory.Lines.Text) > 0, 'the context is in the log: ' + Memory.Lines.Text);
    Assert.IsTrue(Pos('EBoldTraceTest: ' + cMessage, Memory.Lines.Text) > 0,
      'the class and the message are in the log: ' + Memory.Lines.Text);
  finally
    BoldLogManager.UnregisterSink(Sink);
  end;
end;

{$IFDEF FPC}

procedure TTestBoldExceptionTrace.TestTheRaisingRoutineIsFirstInTheCallStack;
var
  CallStack: string;
begin
  CallStack := '';
  try
    RaiseInAKnownRoutine;
  except
    on E: Exception do
      CallStack := BoldExceptionCallStack(E);
  end;
  Assert.IsTrue(Pos('RAISEINAKNOWNROUTINE', UpperCase(FirstLine(CallStack))) > 0,
    'the raising routine first: ' + CallStack);
  Assert.IsTrue(Pos('TESTTHERAISINGROUTINEISFIRSTINTHECALLSTACK', UpperCase(CallStack)) > 0,
    'and its caller: ' + CallStack);
end;

{$ELSE}

procedure TTestBoldExceptionTrace.TestTheProvidersCallStackStartsAtTheRaisingRoutine;
var
  OldGetInfo: TGetExceptionStackInfo;
  OldGetString: TGetStackInfoString;
  OldCleanUp: TCleanUpStackInfo;
  CallStack: string;
begin
  OldGetInfo := Exception.GetExceptionStackInfoProc;
  OldGetString := Exception.GetStackInfoStringProc;
  OldCleanUp := Exception.CleanUpStackInfoProc;
  Exception.GetExceptionStackInfoProc := FakeGetExceptionStackInfo;
  Exception.GetStackInfoStringProc := FakeGetStackInfoString;
  Exception.CleanUpStackInfoProc := FakeCleanUpStackInfo;
  try
    CallStack := '';
    try
      RaiseInAKnownRoutine;
    except
      on E: Exception do
        CallStack := BoldExceptionCallStack(E);
    end;
  finally
    Exception.GetExceptionStackInfoProc := OldGetInfo;
    Exception.GetStackInfoStringProc := OldGetString;
    Exception.CleanUpStackInfoProc := OldCleanUp;
  end;
  Assert.AreEqual('Test.BoldExceptionTrace.RaiseInAKnownRoutine (Line 42, "Test.BoldExceptionTrace.pas")',
    FirstLine(CallStack), 'the raising routine first');
  Assert.AreEqual(2, LineCount(CallStack), 'the raising routine and its caller: ' + CallStack);
end;

procedure TTestBoldExceptionTrace.TestWithoutAProviderTheReportSaysWhereItWasRaised;
var
  OldGetInfo: TGetExceptionStackInfo;
  OldGetString: TGetStackInfoString;
  OldCleanUp: TCleanUpStackInfo;
  Report: string;
begin
  OldGetInfo := Exception.GetExceptionStackInfoProc;
  OldGetString := Exception.GetStackInfoStringProc;
  OldCleanUp := Exception.CleanUpStackInfoProc;
  Exception.GetExceptionStackInfoProc := nil;
  Exception.GetStackInfoStringProc := nil;
  Exception.CleanUpStackInfoProc := nil;
  try
    Report := '';
    try
      RaiseInAKnownRoutine;
    except
      on E: Exception do
        Report := BoldExceptionReport(E);
    end;
  finally
    Exception.GetExceptionStackInfoProc := OldGetInfo;
    Exception.GetStackInfoStringProc := OldGetString;
    Exception.CleanUpStackInfoProc := OldCleanUp;
  end;
  Assert.IsTrue(Pos('Raised at $', Report) > 0, 'the raise address: ' + Report);
  Assert.IsTrue(Pos('module loaded at $' + Format('%p', [nil]), Report) = 0,
    'the load address of the module is known: ' + Report);
end;

{$ENDIF}

initialization
  BoldRegisterTest(TTestBoldExceptionTrace);

end.
