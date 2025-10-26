program UnitTest;

{*******************************
 *                             *
 * Unittests from console      *
 * Useful in automatic builds  *
 *                             *
 *******************************}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  FastMM4 in '..\Source\Extensions\FastMM4\FastMM4.pas',
  FastMM4Messages in '..\Source\Extensions\FastMM4\FastMM4Messages.pas',
  DUnitX.MemoryLeakMonitor.FastMM4 in '..\..\DUnitX\Source\DUnitX.MemoryLeakMonitor.FastMM4.pas',
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  jano_multiplicity in 'jano_multiplicity.pas' {keep comment here to protect the following conditional from being removed by the IDE when adding a unit},
  BoldUMLTypes in '..\Source\Common\UML\BoldUMLTypes.pas',
  BoldBase in '..\Source\Common\Core\BoldBase.pas',
  BoldDefs in '..\Source\Common\Core\BoldDefs.pas',
  BoldCommonConst in '..\Source\Common\Core\BoldCommonConst.pas',
  BoldIndexableList in '..\Source\Common\Support\BoldIndexableList.pas',
  BoldIndex in '..\Source\Common\Support\BoldIndex.pas',
  BoldHashIndexes in '..\Source\Common\Support\BoldHashIndexes.pas',
  BoldUtils in '..\Source\Common\Support\BoldUtils.pas',
  BoldRev in '..\Source\Common\Support\BoldRev.pas',
  BoldContainers in '..\Source\Common\Core\BoldContainers.pas';

{ keep comment here to protect the following conditional from being removed by the IDE when adding a unit }
{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := False;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    if not FindCmdLineSwitch('auto') then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.
