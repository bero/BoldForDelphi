program UnitTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  {$ENDIF }
  DUnitX.TestFramework,
  BoldTestCasePersistence in 'Code\Persistence\BoldTestCasePersistence.pas',
  BoldTestCaseFireDAC in 'Code\Persistence\BoldTestCaseFireDAC.pas',
  Test.BoldUMLTypes in 'Code\Common\Test.BoldUMLTypes.pas',
  Test.BoldAttributes in 'Code\ObjectSpace\Test.BoldAttributes.pas',
  jehoBCBoldTest in 'Code\ObjectSpace\jehoBCBoldTest.pas',
  Test.FetchInvalidAttribute in 'Code\ObjectSpace\Test.FetchInvalidAttribute.pas',
  TestModel1 in 'Code\Main\TestModel1.pas',
  UndoTestModelClasses in 'Code\ObjectSpace\UndoTestModelClasses.pas',
  maan_UndoRedoTestCaseUtils in 'Code\ObjectSpace\maan_UndoRedoTestCaseUtils.pas',
  maan_UndoRedoBase in 'Code\ObjectSpace\maan_UndoRedoBase.pas',
  maan_FetchRefetch in 'Code\ObjectSpace\maan_FetchRefetch.pas',
  maan_Modify in 'Code\ObjectSpace\maan_Modify.pas',
  maan_Undo in 'Code\ObjectSpace\maan_Undo.pas',
  Test.BoldSystem in 'Code\ObjectSpace\Test.BoldSystem.pas',
  Test.BoldFreeStandingValueFactories in 'Code\FreestandingValueSpace\Test.BoldFreeStandingValueFactories.pas',
  Test.BoldDefaultTaggedValues in 'Code\Common\Test.BoldDefaultTaggedValues.pas',
  Test.BoldMemberTypeDictionary in 'Code\RTModel\Test.BoldMemberTypeDictionary.pas',
  Test.BoldGeneratedCodeDictionary in 'Code\RTModel\Test.BoldGeneratedCodeDictionary.pas',
  Test.BoldPersistenceHandleDB in 'Code\Persistence\Test.BoldPersistenceHandleDB.pas',
  Test.BoldPMapperLists in 'Code\PMapper\Test.BoldPMapperLists.pas',
  Test.BoldSQLMappingInfo in 'Code\PMapper\Test.BoldSQLMappingInfo.pas',
  Test.BoldUtils in 'Code\Common\Test.BoldUtils.pas',
  Test.PersistenceFireDAC in 'Code\Persistence\Test.PersistenceFireDAC.pas',
  BoldTestCaseUniDAC in 'Code\Persistence\BoldTestCaseUniDAC.pas',
  Test.PersistenceUniDAC in 'Code\Persistence\Test.PersistenceUniDAC.pas',
  Test.BoldGUIDUtils in 'Code\Common\Test.BoldGUIDUtils.pas';

{$R *.res}

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger: ITestLogger;
{$ENDIF}
begin
  ReportMemoryLeaksOnShutdown := True;
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    // Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    // Create the test runner
    runner := TDUnitX.CreateRunner;
    // Tell the runner to use RTTI to find fixtures
    runner.UseRTTI := True;
    // Tell the runner how we will log things
    // Log to the console window if present
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    // Generate an NUnit compatible XML file
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    // Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    // Pause by default when running manually, use --exit:Continue to skip
    if TDUnitX.Options.ExitBehavior <> TDUnitXExitBehavior.Continue then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
