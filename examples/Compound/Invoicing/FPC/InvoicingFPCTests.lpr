{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program InvoicingFPCTests;
{ FPCUnit console runner for the Invoicing tests, the FPC counterpart of
  ../Tests/InvoicingTests.dpr (DUnitX). Run with --all --format=plain.
  Exit code is non-zero when a test fails. }

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes,
  consoletestrunner,
  TestInvoicingScenarioFPC;

var
  App: TTestRunner;
begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Invoicing FPC tests';
    App.Run;
  finally
    App.Free;
  end;
end.
