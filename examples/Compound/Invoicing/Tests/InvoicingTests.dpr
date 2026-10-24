program InvoicingTests;

{ DUnitX runner for the Invoicing example.

  The checks themselves live in the shared unit InvoicingScenario; the fixture
  Test.InvoicingScenario turns them into test results. The run writes
  dunitx-results.xml next to the exe and returns a non-zero exit code when a
  test fails.

  Usage: InvoicingTests [--run:Test.InvoicingScenario.TTestInvoicingScenario.<Test>]
                        [--consolemode:Quiet|Verbose] [--exit:Pause] }

{$APPTYPE CONSOLE}

{$STRONGLINKTYPES ON}

{$R *.res}

uses
  System.SysUtils,
  FireDAC.ConsoleUI.Wait,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  dmInvoicing in '..\Data\dmInvoicing.pas' {dmInvoicing: TDataModule},
  InvoicingClasses in '..\Model\InvoicingClasses.pas',
  InvoicingScenario in 'InvoicingScenario.pas',
  Test.InvoicingScenario in 'Test.InvoicingScenario.pas';

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
