{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program UnitTestFPC;
{ FPCUnit console runner for the Bold unit tests under Free Pascal, the FPC
  counterpart of ../UnitTest.dpr (DUnitX). The test units are the same source
  for both compilers (see Code/Common/BoldTestFramework.pas); this program
  lists the ones converted so far.

  Build and run on Linux with build.sh next to this file:
    build.sh                   all tests, plain output
    build.sh --suite=TTestX    one fixture
    build.sh --format=xml      XML for CI
  Exit code is non-zero when a test fails. }

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Classes,
  consoletestrunner,
  Test.BoldUtils;

var
  App: TTestRunner;
begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Bold unit tests (FPC)';
    App.Run;
  finally
    App.Free;
  end;
end.
