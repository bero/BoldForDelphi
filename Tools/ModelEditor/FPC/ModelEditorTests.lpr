{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program ModelEditorTests;
{ FPCUnit console runner for the tests of the standalone model editor
  (#128), the Free Pascal counterpart of ../Tests/ModelEditorTests.dpr
  (DUnitX). The test units are the same source for both compilers, through
  UnitTest/Code/Common/BoldTestFramework.pas. Build and run with build.sh
  next to this file; the exit code is non-zero when a test fails. }

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes,
  consoletestrunner,
  Test.BoldModelDocument;

var
  App: TTestRunner;
begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Model editor FPC tests';
    App.Run;
  finally
    App.Free;
  end;
end.
