{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldExceptionLog;

{ Tests of BoldExceptionLog (#130): the files the exception log tries, the
  entry of an exception that no handler took, and the hook into an
  application's message loop that BoldExceptionLogVCL, BoldExceptionLogFMX and
  BoldExceptionLogLCL install. The hook is tried with a stand-in for the
  application, so these tests need no GUI framework.

  The last-resort handler is tried out of process: the test starts this test
  program again with cChildSwitch, its main block calls
  BoldExceptionLogRunChildWhenAsked first, which raises, and the test reads
  the child's log, its output and its exit code. Raising in a unit's
  initialization would not do: Delphi finalizes the units, BoldExceptionLog
  among them, before it reports such an exception.

  This unit switches the exception log's own file off in the test programs:
  the tests that write to Bold's log would otherwise leave a log next to the
  test program. }

interface

uses
  SysUtils,
  BoldTestFramework;

type
  EBoldExceptionLogTest = class(Exception);

  {$IFNDEF FPC}
  [Category('Common')]
  {$ENDIF}
  TTestBoldExceptionLog = class(TBoldTestCase)
  published
    // The program's folder first, then the user's folder for logs; in each
    // the program's name, then the name with the process id
    procedure TestTheLogFilesAreTheProgramsFolderThenTheUsers;
    procedure TestWithoutAUserFolderOnlyTheProgramsFolder;
    procedure TestTheExceptionLogTriesThisProgramsFiles;
    // What the RTL's last-resort handler (System.ExceptProc) is given goes
    // to the log
    procedure TestAnUnhandledExceptionGoesToTheLog;
    procedure TestTheHookLogsAndShowsWhenTheProgramHasNoHandler;
    procedure TestTheHookCallsTheProgramsHandlerInsteadOfShowing;
    // A program sets its own handler after the hook, in its start-up
    procedure TestAHandlerSetAfterTheHookIsCalledWhenHookedAgain;
    procedure TestHookingTwiceLogsOnce;
    procedure TestWithoutAnApplicationNothingIsHooked;
    procedure TestUnhookGivesBackTheProgramsHandler;
    // The second hook comes from the application's queue, after the start-up
    procedure TestTheHookIsRepeatedAfterStartUp;
    procedure TestUnhookTakesBackTheQueuedHook;
    // Out of process: an exception in the main block goes to the log through
    // the installed last-resort handler, then the handler that was there
    // reports it and ends the program
    procedure TestAnExceptionInTheMainBlockGoesToTheLog;
    // Out of process: while this process holds the program's log, a second
    // process writes to <program>-<process id>.log
    procedure TestASecondProcessWritesItsOwnLog;
  end;

{ The test program's main block calls this first. Started with cChildSwitch
  (by the two out-of-process tests), it switches the exception log's file on
  and raises; the RTL's last-resort handler ends the program. Otherwise it
  does nothing }
procedure BoldExceptionLogRunChildWhenAsked;

implementation

uses
  Classes,
  {$IFDEF FPC}
  process,
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  Posix.Stdlib,
  Posix.Unistd, // SysUtils.DeleteFile is inlined from it (H2443 without)
  {$ENDIF}
  {$ENDIF}
  BoldLogInterfaces,
  BoldLogSinks,
  BoldPlatform,
  BoldExceptionLog;

const
  cMessage = 'raised in the message loop';
  cChildSwitch = '--bold-exception-log-child';
  cChildMessage = 'raised in the main block of the child';
  // How long the child may take
  cChildTimeout = 60000;

type
  { Stands in for VCL's, FMX's or the LCL's Application }
  TStandInApplication = class
  public
    Exists: Boolean;
    OnException: TBoldExceptionEvent;
    Shown: Integer;
    Queued: TThreadMethod;
    constructor Create;
    { What the message loop does when it starts }
    procedure RunTheQueue;
  end;

  TStandInExceptionLog = class(TBoldApplicationExceptionLog)
  private
    FApplication: TStandInApplication;
  protected
    function ApplicationExists: Boolean; override;
    function GetOnException: TBoldExceptionEvent; override;
    procedure SetOnException(const AHandler: TBoldExceptionEvent); override;
    procedure ShowException(E: Exception); override;
    procedure QueueHook; override;
    procedure UnqueueHook; override;
  public
    constructor Create(AApplication: TStandInApplication);
  end;

  { The handler a program sets in Application.OnException }
  TProgramHandler = class
  public
    Calls: Integer;
    procedure HandleException(Sender: TObject; E: Exception);
  end;

  { An application, the exception log's hook into it, a program's handler and
    a log in memory, for one test }
  TScene = class
  public
    Application: TStandInApplication;
    ExceptionLog: TStandInExceptionLog;
    ProgramsHandler: TProgramHandler;
    Log: TBoldMemoryLogSink;
    LogRef: IBoldLogSink;
    constructor Create;
    destructor Destroy; override;
    { Raises in a routine and hands the exception to Application.OnException,
      as the message loop does }
    procedure RaiseInTheMessageLoop;
    function LogText: string;
    function LoggedReports: Integer;
  end;

constructor TStandInApplication.Create;
begin
  inherited Create;
  Exists := True;
end;

procedure TStandInApplication.RunTheQueue;
var
  Method: TThreadMethod;
begin
  Method := Queued;
  Queued := nil;
  if Assigned(Method) then
    Method;
end;

constructor TStandInExceptionLog.Create(AApplication: TStandInApplication);
begin
  inherited Create;
  FApplication := AApplication;
end;

function TStandInExceptionLog.ApplicationExists: Boolean;
begin
  Result := FApplication.Exists;
end;

function TStandInExceptionLog.GetOnException: TBoldExceptionEvent;
begin
  Result := FApplication.OnException;
end;

procedure TStandInExceptionLog.SetOnException(const AHandler: TBoldExceptionEvent);
begin
  FApplication.OnException := AHandler;
end;

procedure TStandInExceptionLog.ShowException(E: Exception);
begin
  Inc(FApplication.Shown);
end;

procedure TStandInExceptionLog.QueueHook;
begin
  FApplication.Queued := Hook;
end;

procedure TStandInExceptionLog.UnqueueHook;
begin
  FApplication.Queued := nil;
end;

procedure TProgramHandler.HandleException(Sender: TObject; E: Exception);
begin
  Inc(Calls);
end;

constructor TScene.Create;
begin
  inherited Create;
  Application := TStandInApplication.Create;
  ExceptionLog := TStandInExceptionLog.Create(Application);
  ProgramsHandler := TProgramHandler.Create;
  Log := TBoldMemoryLogSink.Create;
  LogRef := Log;
  BoldLogManager.RegisterSink(LogRef);
end;

destructor TScene.Destroy;
begin
  BoldLogManager.UnregisterSink(LogRef);
  LogRef := nil;
  ExceptionLog.Free;
  ProgramsHandler.Free;
  Application.Free;
  inherited;
end;

procedure RaiseInARoutine;
begin
  raise EBoldExceptionLogTest.Create(cMessage);
end;

procedure TScene.RaiseInTheMessageLoop;
begin
  try
    RaiseInARoutine;
  except
    on E: Exception do
      if Assigned(Application.OnException) then
        Application.OnException(Application, E);
  end;
end;

function TScene.LogText: string;
begin
  Result := Log.Lines.Text;
end;

function TScene.LoggedReports: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Log.Lines.Count - 1 do
    if Pos('EBoldExceptionLogTest: ' + cMessage, Log.Lines[i]) > 0 then
      Inc(Result);
end;

procedure RaiseInTheChild;
var
  E: EBoldExceptionLogTest;
begin
  E := EBoldExceptionLogTest.Create(cChildMessage);
  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  // The program ends without freeing it: FastMM4's leak report
  // (check_memory.ps1, Windows) is told so. The message is a literal, no
  // copy. Delphi declares RegisterExpectedMemoryLeak on Windows only
  RegisterExpectedMemoryLeak(E);
  {$ENDIF}
  {$ENDIF}
  raise E;
end;

procedure BoldExceptionLogRunChildWhenAsked;
begin
  if ParamStr(1) <> cChildSwitch then
    Exit;
  BoldExceptionLogSink.Enabled := True;
  RaiseInTheChild;
end;

{ Starts this test program with cChildSwitch and waits for it: its exit code,
  and in AOutput what it wrote to standard output and standard error }
function RunTheChild(out AOutput: string): Integer;
{$IFDEF FPC}
var
  Captured: AnsiString;
begin
  RunCommandIndir('', AnsiString(ParamStr(0)), [cChildSwitch], Captured, Result, [poStderrToOutPut]);
  {$IFDEF UNIX}
  // The wait status, of which the exit code is the second byte (WEXITSTATUS)
  Result := (Result shr 8) and $FF;
  {$ENDIF}
  AOutput := string(Captured);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  OutputFile, CommandLine: string;
  Security: TSecurityAttributes;
  OutputHandle: THandle;
  Startup: TStartupInfo;
  Info: TProcessInformation;
  Code: DWORD;
begin
  OutputFile := BoldTestTempFileName('.txt');
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := True;
  OutputHandle := CreateFile(PChar(OutputFile), GENERIC_WRITE, FILE_SHARE_READ, @Security,
    CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if OutputHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    FillChar(Startup, SizeOf(Startup), 0);
    Startup.cb := SizeOf(Startup);
    Startup.dwFlags := STARTF_USESTDHANDLES;
    Startup.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    Startup.hStdOutput := OutputHandle;
    Startup.hStdError := OutputHandle;
    CommandLine := '"' + ParamStr(0) + '" ' + cChildSwitch;
    UniqueString(CommandLine);
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, True, CREATE_NO_WINDOW, nil, nil, Startup, Info) then
      RaiseLastOSError;
    try
      if WaitForSingleObject(Info.hProcess, cChildTimeout) <> WAIT_OBJECT_0 then
      begin
        TerminateProcess(Info.hProcess, 1);
        raise Exception.Create('the child did not end');
      end;
      GetExitCodeProcess(Info.hProcess, Code);
      Result := Integer(Code);
    finally
      CloseHandle(Info.hThread);
      CloseHandle(Info.hProcess);
    end;
  finally
    CloseHandle(OutputHandle);
  end;
  AOutput := BoldTestReadAllText(OutputFile);
  SysUtils.DeleteFile(OutputFile);
end;
{$ELSE}
var
  OutputFile: string;
  Status: Integer;
begin
  OutputFile := BoldTestTempFileName('.txt');
  Status := _system(MarshaledAString(UTF8String('"' + ParamStr(0) + '" ' + cChildSwitch +
    ' > "' + OutputFile + '" 2>&1')));
  // WEXITSTATUS
  Result := (Status shr 8) and $FF;
  AOutput := BoldTestReadAllText(OutputFile);
  SysUtils.DeleteFile(OutputFile);
end;
{$ENDIF}
{$ENDIF}

{ The program's log files that a child may leave next to this program:
  <program>.log and <program>-<process id>.log. SysUtils' routines are named:
  the Windows unit has a DeleteFile and a FindClose of its own }
procedure DeleteTheChildLogs;
var
  Found: TSearchRec;
  Folder: string;
begin
  Folder := ExtractFilePath(ExpandFileName(ParamStr(0)));
  SysUtils.DeleteFile(ChangeFileExt(ExpandFileName(ParamStr(0)), '.log'));
  if FindFirst(ChangeFileExt(ExpandFileName(ParamStr(0)), '') + '-*.log', faAnyFile, Found) = 0 then
  try
    repeat
      SysUtils.DeleteFile(Folder + Found.Name);
    until FindNext(Found) <> 0;
  finally
    SysUtils.FindClose(Found);
  end;
end;

{ The text of AFileName; empty when there is no such file }
function TextIfThere(const AFileName: string): string;
begin
  if FileExists(AFileName) then
    Result := BoldTestReadAllText(AFileName)
  else
    Result := '';
end;

{ The log a second process wrote: <program>-<its process id>.log }
function TheSecondProcessesLog: string;
var
  Found: TSearchRec;
begin
  Result := '';
  if FindFirst(ChangeFileExt(ExpandFileName(ParamStr(0)), '') + '-*.log', faAnyFile, Found) = 0 then
  try
    Result := ExtractFilePath(ExpandFileName(ParamStr(0))) + Found.Name;
  finally
    SysUtils.FindClose(Found);
  end;
end;

function Joined(const ANames: TArray<string>): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(ANames) do
    Result := Result + ANames[i] + sLineBreak;
end;

{ TTestBoldExceptionLog }

procedure TTestBoldExceptionLog.TestTheLogFilesAreTheProgramsFolderThenTheUsers;
var
  ProgramFolder, UserFolder: string;
  Names: TArray<string>;
begin
  ProgramFolder := IncludeTrailingPathDelimiter(BoldTestTempDir) + 'programs';
  UserFolder := IncludeTrailingPathDelimiter(BoldTestTempDir) + 'logs';
  Names := BoldLogFileNames(ProgramFolder + PathDelim + 'Editor.exe', 4711, UserFolder);
  Assert.AreEqual(4, Integer(Length(Names)), Joined(Names));
  Assert.AreEqual(ProgramFolder + PathDelim + 'Editor.log', Names[0]);
  Assert.AreEqual(ProgramFolder + PathDelim + 'Editor-4711.log', Names[1]);
  Assert.AreEqual(UserFolder + PathDelim + 'Editor' + PathDelim + 'Editor.log', Names[2]);
  Assert.AreEqual(UserFolder + PathDelim + 'Editor' + PathDelim + 'Editor-4711.log', Names[3]);
end;

procedure TTestBoldExceptionLog.TestWithoutAUserFolderOnlyTheProgramsFolder;
var
  ProgramFolder: string;
  Names: TArray<string>;
begin
  ProgramFolder := IncludeTrailingPathDelimiter(BoldTestTempDir) + 'programs';
  // A program file without an extension, as on Linux
  Names := BoldLogFileNames(ProgramFolder + PathDelim + 'Editor', 4711, '');
  Assert.AreEqual(2, Integer(Length(Names)), Joined(Names));
  Assert.AreEqual(ProgramFolder + PathDelim + 'Editor.log', Names[0]);
  Assert.AreEqual(ProgramFolder + PathDelim + 'Editor-4711.log', Names[1]);
end;

procedure TTestBoldExceptionLog.TestTheExceptionLogTriesThisProgramsFiles;
var
  Names: TArray<string>;
begin
  Names := BoldExceptionLogFileNames;
  Assert.AreEqual(ChangeFileExt(ExpandFileName(ParamStr(0)), '.log'), Names[0], Joined(Names));
  Assert.AreEqual(ChangeFileExt(ExpandFileName(ParamStr(0)), '') + '-' + IntToStr(BoldCurrentProcessId) + '.log',
    Names[1], Joined(Names));
  Assert.IsNotNull(BoldExceptionLogSink, 'the exception log');
end;

procedure TTestBoldExceptionLog.TestAnUnhandledExceptionGoesToTheLog;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    try
      RaiseInARoutine;
    except
      on E: Exception do
        BoldLogUnhandledException(E, ExceptAddr, 0, nil);
    end;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.Contains(Scene.LogText, 'no handler', 'says that no handler took it');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestTheHookLogsAndShowsWhenTheProgramHasNoHandler;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.ExceptionLog.Hook;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.Application.Shown, 'shown');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestTheHookCallsTheProgramsHandlerInsteadOfShowing;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.Application.OnException := Scene.ProgramsHandler.HandleException;
    Scene.ExceptionLog.Hook;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.ProgramsHandler.Calls, 'the program''s handler');
    Assert.AreEqual(0, Scene.Application.Shown, 'not shown by the hook');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestAHandlerSetAfterTheHookIsCalledWhenHookedAgain;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.ExceptionLog.Hook;
    Scene.Application.OnException := Scene.ProgramsHandler.HandleException;
    Scene.ExceptionLog.Hook;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.ProgramsHandler.Calls, 'the program''s handler');
    Assert.AreEqual(0, Scene.Application.Shown, 'not shown by the hook');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestHookingTwiceLogsOnce;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.ExceptionLog.Hook;
    Scene.ExceptionLog.Hook;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.Application.Shown, 'shown once');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestWithoutAnApplicationNothingIsHooked;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.Application.Exists := False;
    Scene.ExceptionLog.Hook;
    Assert.IsFalse(Assigned(Scene.Application.OnException), 'no handler set');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestUnhookGivesBackTheProgramsHandler;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.Application.OnException := Scene.ProgramsHandler.HandleException;
    Scene.ExceptionLog.Hook;
    Scene.ExceptionLog.Unhook;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(0, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.ProgramsHandler.Calls, 'the program''s handler');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestTheHookIsRepeatedAfterStartUp;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.ExceptionLog.HookNowAndAfterStartUp;
    // The program's start-up, a form's OnCreate, sets its own handler
    Scene.Application.OnException := Scene.ProgramsHandler.HandleException;
    Scene.Application.RunTheQueue;
    Scene.RaiseInTheMessageLoop;
    Assert.AreEqual(1, Scene.LoggedReports, Scene.LogText);
    Assert.AreEqual(1, Scene.ProgramsHandler.Calls, 'the program''s handler');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestUnhookTakesBackTheQueuedHook;
var
  Scene: TScene;
begin
  Scene := TScene.Create;
  try
    Scene.ExceptionLog.HookNowAndAfterStartUp;
    Scene.ExceptionLog.Unhook;
    Assert.IsFalse(Assigned(Scene.Application.Queued), 'nothing left in the queue');
    Assert.IsFalse(Assigned(Scene.Application.OnException), 'no handler set');
  finally
    Scene.Free;
  end;
end;

procedure TTestBoldExceptionLog.TestAnExceptionInTheMainBlockGoesToTheLog;
var
  Output, Log: string;
  Code: Integer;
begin
  // A GUI test program has no main block that knows cChildSwitch
  if not IsConsole then
    Assert.Pass('only in a console test program');
  DeleteTheChildLogs;
  try
    Code := RunTheChild(Output);
    Log := TextIfThere(BoldExceptionLogFileNames[0]);
    Assert.Contains(Log, 'no handler took', 'the child''s log; its output: ' + Output);
    Assert.Contains(Log, 'EBoldExceptionLogTest: ' + cChildMessage, 'the class and the message');
    {$IFDEF FPC}
    Assert.Contains(UpperCase(Log), 'RAISEINTHECHILD', 'the frames Free Pascal gives ExceptProc: ' + Log);
    // Then SysUtils' handler, which was there before: it reports the
    // exception, and the RTL ends the program with 217
    Assert.Contains(Output, 'An unhandled exception occurred', 'the RTL''s report: ' + Output);
    Assert.AreEqual(217, Code, 'the exit code');
    {$ELSE}
    // The test program has no stack-trace provider
    Assert.Contains(Log, 'Raised at $', 'where it was raised: ' + Log);
    // Then SysUtils' handler, which was there before: it reports the
    // exception and ends the program with 1
    Assert.Contains(Output, 'EBoldExceptionLogTest', 'the RTL''s report: ' + Output);
    Assert.AreEqual(1, Code, 'the exit code');
    {$ENDIF}
  finally
    DeleteTheChildLogs;
  end;
end;

procedure TTestBoldExceptionLog.TestASecondProcessWritesItsOwnLog;
var
  Holder: IBoldLogSink;
  Output, SecondLog: string;
begin
  if not IsConsole then
    Assert.Pass('only in a console test program');
  DeleteTheChildLogs;
  try
    // This process holds the program's log, as a first instance does
    Holder := TBoldFileLogSink.Create([BoldExceptionLogFileNames[0]]);
    Holder.WriteLog('the first instance', llInfo, Now, 0);
    RunTheChild(Output);
    SecondLog := TheSecondProcessesLog;
    Assert.IsTrue(SecondLog <> '', 'a log <program>-<process id>.log; the child''s output: ' + Output);
    Assert.Contains(TextIfThere(SecondLog), cChildMessage, 'the second process''s exception in it');
    Holder := nil;
    Assert.IsFalse(Pos(cChildMessage, TextIfThere(BoldExceptionLogFileNames[0])) > 0,
      'nothing of the second process in the first one''s log');
  finally
    Holder := nil;
    DeleteTheChildLogs;
  end;
end;

initialization
  BoldExceptionLogSink.Enabled := False;
  BoldRegisterTest(TTestBoldExceptionLog);

end.
