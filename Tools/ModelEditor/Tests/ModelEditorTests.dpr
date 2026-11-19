program ModelEditorTests;

{ DUnitX runner for the tests of the standalone model editor (#128), the
  Delphi counterpart of ../FPC/ModelEditorTests.lpr. The test units are the
  same source for both compilers, through
  UnitTest/Code/Common/BoldTestFramework.pas. The run writes
  dunitx-results.xml next to the exe and returns a non-zero exit code when a
  test fails. Build with build.ps1 next to this file.

  Usage: ModelEditorTests [--run:Test.BoldModelDocument.TTestBoldModelDocument.<Test>]
                          [--consolemode:Quiet|Verbose] [--exit:Pause] }

{$APPTYPE CONSOLE}

{$STRONGLINKTYPES ON}

{$R *.res}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  BoldModelDocument in '..\Core\BoldModelDocument.pas',
  BoldTestFramework in '..\..\..\UnitTest\Code\Common\BoldTestFramework.pas',
  Test.BoldModelDocument in 'Test.BoldModelDocument.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;

begin
  try
    TDUnitX.CheckCommandLine;
    Runner := TDUnitX.CreateRunner;
    Runner.UseRTTI := True;

    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      Logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      Runner.AddLogger(Logger);
    end;

    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);
    Results := Runner.Execute;
    if not Results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    if TDUnitX.Options.ExitBehavior <> TDUnitXExitBehavior.Continue then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
end.
