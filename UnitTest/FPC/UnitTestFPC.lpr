{$mode delphiunicode}
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
  BoldTestDMZeos, // the database behind dmBoldTest
  maan_UndoRedoZeos, // the database behind dmUndoRedo
  BoldTestCaseDBZeos, // the database behind TBoldTestCaseDB
  Test.BoldAbstractObjectUpgrader,
  Test.BoldAbstractObjectUpgraderHandle,
  Test.BoldAbstractSnooper,
  Test.BoldAttributes,
  Test.BoldBatchQueries,
  Test.BoldBase,
  Test.BoldBase64,
  Test.BoldClock,
  Test.BoldCollections,
  Test.BoldCondition,
  Test.BoldContainers,
  Test.BoldDbCopy,
  Test.BoldDbEvolutor,
  Test.BoldDBInterfacesLogging,
  Test.BoldDbParameter,
  Test.BoldDbValidator,
  Test.BoldDefaultId,
  Test.BoldDefaultTaggedValues,
  Test.BoldDefaultXMLStreaming,
  Test.BoldDefs,
  Test.BoldDeriver,
  Test.BoldDirectoryTraverser,
  Test.BoldElements,
  Test.BoldEnvironment,
  Test.BoldEventQueue,
  Test.BoldExternalizedReferences,
  Test.BoldFilteredHandle,
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
  Test.BoldLinks,
  Test.BoldListHandle,
  Test.BoldLogHandler,
  Test.BoldLogInterfaces,
  Test.BoldLogReceiverInterface,
  Test.BoldMD5,
  Test.BoldMemberTypeDictionary,
  Test.BoldMeta,
  Test.BoldModel,
  Test.BoldNameExpander,
  Test.BoldNamedValueList,
  Test.BoldOclClasses,
  Test.BoldOclError,
  Test.BoldOclEvaluation,
  Test.BoldOclLightWeightNodes,
  Test.BoldOclObjectTimeStamp,
  Test.BoldOclVariables,
  Test.BoldOldValueHandler,
  Test.BoldPMapperLists,
  Test.BoldPMappersDefault,
  Test.BoldPMTransactionGuards,
  Test.BoldPersistenceController,
  Test.BoldPersistenceHandle,
  Test.BoldPersistenceHandleDB,
  Test.BoldPersistenceZeos,
  Test.BoldPlatform,
  Test.BoldQueue,
  Test.BoldRegEx,
  Test.BoldSQLDatabaseConfig,
  Test.BoldSQLMappingInfo,
  Test.BoldSSExcept,
  Test.BoldSharedStrings,
  Test.BoldSortedHandle,
  Test.BoldSorter,
  Test.BoldSqlNodes,
  Test.BoldSqlParameterization,
  Test.BoldSqlParameterizationBlocks,
  Test.BoldStubs,
  Test.BoldSubscribableCollection,
  Test.BoldSubscription,
  Test.BoldSystem,
  Test.BoldSystemPersistent,
  Test.BoldTaggedValueList,
  Test.BoldTaggedValueSupport,
  Test.BoldThreadSafeLog,
  Test.BoldThreadSafeQueue,
  Test.BoldTypeNameDictionary,
  Test.BoldUMLModelValidator,
  Test.BoldUMLTaggedValues,
  Test.BoldUMLTypes,
  Test.BoldURI,
  Test.BoldUndoHandler,
  Test.BoldUnloaderHandle,
  Test.BoldUpdatePrecondition,
  Test.BoldUtils,
  Test.BoldXMLStreaming,
  Test.BoldZeosAdapter,
  Test.FetchInvalidAttribute,
  Test.PersistenceScenariosZeos;

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
