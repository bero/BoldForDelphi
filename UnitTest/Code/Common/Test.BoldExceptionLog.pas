{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit Test.BoldExceptionLog;

{ Tests of BoldExceptionLog (#130): the files the exception log tries, the
  entry of an exception that no handler took, and the hook into an
  application's message loop that BoldExceptionLogVCL, BoldExceptionLogFMX and
  BoldExceptionLogLCL install. The hook is tried with a stand-in for the
  application, so these tests need no GUI framework.

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
  end;

implementation

uses
  Classes,
  BoldLogInterfaces,
  BoldLogSinks,
  BoldPlatform,
  BoldExceptionLog;

const
  cMessage = 'raised in the message loop';

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

initialization
  BoldExceptionLogSink.Enabled := False;
  BoldRegisterTest(TTestBoldExceptionLog);

end.
