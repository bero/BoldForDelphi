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
    // A deep stack keeps more than the 16 callers Free Pascal records by
    // default: one that comes through a GUI's message loop is that deep
    procedure TestADeepCallStackKeepsMoreThanSixteenFrames;
    {$ELSE}
    // A provider's text, without its capture routines and the RTL's raise
    // routines, which come before the raising routine
    procedure TestTheProvidersCallStackStartsAtTheRaisingRoutine;
    // No provider: the report says where the exception was raised
    procedure TestWithoutAProviderTheReportSaysWhereItWasRaised;
    // An exception raised while another one unwinds: its own raising
    // routine first (found by the VM's review with a real Stacker run)
    procedure TestARaiseWhileAnotherUnwindsStartsAtItsOwnRaisingRoutine;
    // A caller whose name contains RaiseException is not taken for the RTL's
    // raise routine
    procedure TestACallerNamedRaiseExceptionStaysInTheCallStack;
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

var
  Depth: Integer;

{ Calls itself ADepth times, then raises }
procedure RaiseAtDepth(ADepth: Integer);
begin
  if ADepth = 0 then
    raise EBoldTraceTest.Create(cMessage);
  RaiseAtDepth(ADepth - 1);
  // Not a tail call: the frames stay
  Depth := ADepth;
end;

{ How many lines of AText contain AName, in upper case }
function LinesNaming(const AText, AName: string): Integer;
var
  Lines: TStringList;
  i: Integer;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := UpperCase(AText);
    for i := 0 to Lines.Count - 1 do
      if Pos(UpperCase(AName), Lines[i]) > 0 then
        Inc(Result);
  finally
    Lines.Free;
  end;
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

{ Stacker's text for an exception raised in a destructor while another
  exception unwinds (taken from a real Win64 run): the other exception's raise
  frame comes further down }
function FakeNestedRaiseStackInfoString(Info: Pointer): string;
begin
  Result :=
    'Provider.Capture.CaptureFrames (Line 30, "Provider.Capture.pas")' + sLineBreak +
    'System.SysUtils.RaiseExceptObject' + sLineBreak +
    'System.@InternalRaiseAtExcept' + sLineBreak +
    'System.@RaiseExcept' + sLineBreak +
    'Unit1.TThing.Destroy (Line 23, "Unit1.pas")' + sLineBreak +
    'System.TObject.Free' + sLineBreak +
    'Unit1.DoWork (Line 34, "Unit1.pas")' + sLineBreak +
    'System.@DelphiExceptionHandler' + sLineBreak +
    'System.@InternalRaiseAtExcept' + sLineBreak +
    'System.@RaiseExcept' + sLineBreak +
    'Unit1.FailFirst (Line 18, "Unit1.pas")' + sLineBreak +
    'Unit1.DoWork (Line 32, "Unit1.pas")';
end;

{ A caller whose name contains the word RaiseException }
function FakeNamedCallerStackInfoString(Info: Pointer): string;
begin
  Result :=
    'Provider.Capture.CaptureFrames (Line 30, "Provider.Capture.pas")' + sLineBreak +
    'System.SysUtils.RaiseExceptObject' + sLineBreak +
    'System.@RaiseExcept' + sLineBreak +
    'Unit1.Inner (Line 40, "Unit1.pas")' + sLineBreak +
    'Unit1.RaiseExceptionIfInvalid (Line 45, "Unit1.pas")' + sLineBreak +
    'Unit1.Main (Line 69, "Unit1.pas")';
end;

{ BoldExceptionCallStack of an exception raised while the fake provider with
  the text of AGetString is installed }
function CallStackFromProvider(AGetString: TGetStackInfoString): string;
var
  OldGetInfo: TGetExceptionStackInfo;
  OldGetString: TGetStackInfoString;
  OldCleanUp: TCleanUpStackInfo;
begin
  Result := '';
  OldGetInfo := Exception.GetExceptionStackInfoProc;
  OldGetString := Exception.GetStackInfoStringProc;
  OldCleanUp := Exception.CleanUpStackInfoProc;
  Exception.GetExceptionStackInfoProc := FakeGetExceptionStackInfo;
  Exception.GetStackInfoStringProc := AGetString;
  Exception.CleanUpStackInfoProc := FakeCleanUpStackInfo;
  try
    try
      RaiseInAKnownRoutine;
    except
      on E: Exception do
        Result := BoldExceptionCallStack(E);
    end;
  finally
    Exception.GetExceptionStackInfoProc := OldGetInfo;
    Exception.GetStackInfoStringProc := OldGetString;
    Exception.CleanUpStackInfoProc := OldCleanUp;
  end;
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

procedure TTestBoldExceptionTrace.TestADeepCallStackKeepsMoreThanSixteenFrames;
var
  CallStack: string;
begin
  CallStack := '';
  try
    RaiseAtDepth(40);
  except
    on E: Exception do
      CallStack := BoldExceptionCallStack(E);
  end;
  Assert.IsTrue(LinesNaming(CallStack, 'RAISEATDEPTH') > 40,
    'the 41 calls of RaiseAtDepth: ' + IntToStr(LinesNaming(CallStack, 'RAISEATDEPTH')) + ' in ' + CallStack);
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

procedure TTestBoldExceptionTrace.TestARaiseWhileAnotherUnwindsStartsAtItsOwnRaisingRoutine;
var
  CallStack: string;
begin
  CallStack := CallStackFromProvider(FakeNestedRaiseStackInfoString);
  Assert.AreEqual('Unit1.TThing.Destroy (Line 23, "Unit1.pas")', FirstLine(CallStack),
    'the routine that raised this exception first: ' + CallStack);
end;

procedure TTestBoldExceptionTrace.TestACallerNamedRaiseExceptionStaysInTheCallStack;
var
  CallStack: string;
begin
  CallStack := CallStackFromProvider(FakeNamedCallerStackInfoString);
  Assert.AreEqual('Unit1.Inner (Line 40, "Unit1.pas")', FirstLine(CallStack),
    'the raising routine first: ' + CallStack);
  Assert.AreEqual(3, LineCount(CallStack), 'the raising routine and its two callers: ' + CallStack);
end;

{$ENDIF}

initialization
  BoldRegisterTest(TTestBoldExceptionTrace);

end.
