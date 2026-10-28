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
  Test.BoldAbstractObjectUpgrader,
  Test.BoldAbstractObjectUpgraderHandle,
  Test.BoldBase,
  Test.BoldBase64,
  Test.BoldClock,
  Test.BoldCollections,
  Test.BoldCondition,
  Test.BoldContainers,
  Test.BoldDefaultId,
  Test.BoldDefaultTaggedValues,
  Test.BoldDefs,
  Test.BoldDeriver,
  Test.BoldElements,
  Test.BoldEnvironment,
  Test.BoldEventQueue,
  Test.BoldExternalizedReferences,
  Test.BoldFreeStandingValueFactories,
  Test.BoldFreeStandingValues,
  Test.BoldGUIDUtils,
  Test.BoldGeneratedCodeDictionary,
  Test.BoldGlobalId,
  Test.BoldGuard,
  Test.BoldHashIndexes,
  Test.BoldId,
  Test.BoldIndexCollection,
  Test.BoldIsoDateTime,
  Test.BoldLogHandler,
  Test.BoldLogInterfaces,
  Test.BoldLogReceiverInterface,
  Test.BoldMD5,
  Test.BoldMemberTypeDictionary,
  Test.BoldNameExpander,
  Test.BoldNamedValueList,
  Test.BoldOclClasses,
  Test.BoldOclError,
  Test.BoldOclLightWeightNodes,
  Test.BoldPMapperLists,
  Test.BoldPersistenceController,
  Test.BoldPersistenceHandleDB,
  Test.BoldQueue,
  Test.BoldSQLDatabaseConfig,
  Test.BoldSQLMappingInfo,
  Test.BoldSSExcept,
  Test.BoldSharedStrings,
  Test.BoldSortedHandle,
  Test.BoldSorter,
  Test.BoldSqlNodes,
  Test.BoldStubs,
  Test.BoldSubscribableCollection,
  Test.BoldSubscription,
  Test.BoldTaggedValueList,
  Test.BoldTaggedValueSupport,
  Test.BoldThreadSafeLog,
  Test.BoldTypeNameDictionary,
  Test.BoldUMLTaggedValues,
  Test.BoldUMLTypes,
  Test.BoldURI,
  Test.BoldUpdatePrecondition,
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
