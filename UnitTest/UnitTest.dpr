program UnitTest;

{╔════════════════════════════════════════════════════════════════════════════╗
 ║ DUnitX Command Line Options                                                ║
 ╠════════════════════════════════════════════════════════════════════════════╣
 ║   --options:value      -opt:value    Options File                          ║
 ║   --hidebanner         -b            Hide the License Banner               ║
 ║   --xmlfile:value      -xml:value    XML output file path                  ║
 ║   --runlist:value      -rl:value     File listing tests to run             ║
 ║   --run:value          -r:value      Tests to run (comma-separated)        ║
 ║   --include:value      -i:value      Categories to include                 ║
 ║   --exclude:value      -e:value      Categories to exclude                 ║
 ║   --dontshowignored    -dsi          Don't show ignored tests              ║
 ║   --loglevel:value     -l:value      Logging: Information, Warning, Error  ║
 ║   --exitbehavior:value -exit:value   Exit: Continue (default), Pause       ║
 ║   --consolemode:value  -cm:value     Console: Off, Quiet, Verbose (default)║
 ║   -h  -?                             Show Usage                            ║
 ╠════════════════════════════════════════════════════════════════════════════╣
 ║ Examples:                                                                  ║
 ║   UnitTest.exe --include:Quick       Run only Quick category tests         ║
 ║   UnitTest.exe --run:TTFoo.TestBar   Run specific test                     ║
 ║   UnitTest.exe --exit:Pause          Pause before exit                     ║
 ╚════════════════════════════════════════════════════════════════════════════╝}

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}

{$STRONGLINKTYPES ON}

uses
  TestRunner in 'Code\Main\TestRunner.pas',
  BoldTestCasePersistence in 'Code\Persistence\BoldTestCasePersistence.pas',
  BoldTestDatabaseConfig in 'Code\Persistence\BoldTestDatabaseConfig.pas',
  BoldTestCaseFireDAC in 'Code\Persistence\BoldTestCaseFireDAC.pas',
  BoldTestCaseUniDAC in 'Code\Persistence\BoldTestCaseUniDAC.pas',
  Test.BoldUMLTypes in 'Code\Common\Test.BoldUMLTypes.pas',
  jehoBCBoldTest in 'Code\ObjectSpace\jehoBCBoldTest.pas',
  Test.FetchInvalidAttribute in 'Code\ObjectSpace\Test.FetchInvalidAttribute.pas',
  TestModel1 in 'Code\Main\TestModel1.pas',
  UndoTestModelClasses in 'Code\ObjectSpace\UndoTestModelClasses.pas',
  maan_UndoRedoTestCaseUtils in 'Code\ObjectSpace\maan_UndoRedoTestCaseUtils.pas',
  maan_UndoRedoBase in 'Code\ObjectSpace\maan_UndoRedoBase.pas',
  maan_FetchRefetch in 'Code\ObjectSpace\maan_FetchRefetch.pas',
  dmBoldTest in 'Code\ObjectSpace\dmBoldTest.pas' {BoldTestDM: TDataModule},
  Test.BoldUndoHandler in 'Code\ObjectSpace\Test.BoldUndoHandler.pas',
  Test.BoldOclError in 'Code\ObjectSpace\Test.BoldOclError.pas',
  Test.BoldOclClasses in 'Code\ObjectSpace\Test.BoldOclClasses.pas',
  Test.BoldOclLightWeightNodes in 'Code\ObjectSpace\Test.BoldOclLightWeightNodes.pas',
  Test.BoldOclEvaluation in 'Code\ObjectSpace\Test.BoldOclEvaluation.pas',
  Test.BoldSystem in 'Code\ObjectSpace\Test.BoldSystem.pas',
  Test.BoldLinks in 'Code\ObjectSpace\Test.BoldLinks.pas',
  Test.BoldFreeStandingValueFactories in 'Code\FreestandingValueSpace\Test.BoldFreeStandingValueFactories.pas',
  Test.BoldFreeStandingValues in 'Code\FreestandingValueSpace\Test.BoldFreeStandingValues.pas',
  Test.BoldDefaultTaggedValues in 'Code\Common\Test.BoldDefaultTaggedValues.pas',
  Test.BoldMemberTypeDictionary in 'Code\RTModel\Test.BoldMemberTypeDictionary.pas',
  Test.BoldGeneratedCodeDictionary in 'Code\RTModel\Test.BoldGeneratedCodeDictionary.pas',
  Test.BoldPersistenceHandleDB in 'Code\Persistence\Test.BoldPersistenceHandleDB.pas',
  Test.BoldUpdatePrecondition in 'Code\Persistence\Test.BoldUpdatePrecondition.pas',
  Test.BoldPersistenceController in 'Code\Persistence\Test.BoldPersistenceController.pas',
  Test.BoldAbstractObjectUpgraderHandle in 'Code\Persistence\Test.BoldAbstractObjectUpgraderHandle.pas',
  Test.BoldPMapperLists in 'Code\PMapper\Test.BoldPMapperLists.pas',
  Test.BoldSQLMappingInfo in 'Code\PMapper\Test.BoldSQLMappingInfo.pas',
  Test.BoldSqlNodes in 'Code\PMapper\Test.BoldSqlNodes.pas',
  Test.BoldPMappersDefault in 'Code\PMapper\Test.BoldPMappersDefault.pas',
  Test.BoldAbstractObjectUpgrader in 'Code\PMapper\Test.BoldAbstractObjectUpgrader.pas',
  Test.BoldSQLDatabaseConfig in 'Code\PMapper\Test.BoldSQLDatabaseConfig.pas',
  Test.BoldUtils in 'Code\Common\Test.BoldUtils.pas',
  Test.PersistenceFireDAC in 'Code\Persistence\Test.PersistenceFireDAC.pas',
  Test.BoldGUIDUtils in 'Code\Common\Test.BoldGUIDUtils.pas',
  Test.BoldIsoDateTime in 'Code\Common\Test.BoldIsoDateTime.pas',
  Test.BoldMD5 in 'Code\ProductControl\Test.BoldMD5.pas',
  Test.BoldThreadSafeQueue in 'Code\Propagator\Test.BoldThreadSafeQueue.pas',
  Test.BoldThreadSafeLog in 'Code\Common\Test.BoldThreadSafeLog.pas',
  Test.BoldLogInterfaces in 'Code\Common\Test.BoldLogInterfaces.pas',
  Test.BoldLogHandler in 'Code\Common\Test.BoldLogHandler.pas',
  Test.BoldGuard in 'Code\Common\Test.BoldGuard.pas',
  Test.BoldQueue in 'Code\Common\Test.BoldQueue.pas',
  Test.BoldContainers in 'Code\Common\Test.BoldContainers.pas',
  Test.BoldExternalizedReferences in 'Code\Common\Test.BoldExternalizedReferences.pas',
  Test.BoldStubs in 'Code\Common\Test.BoldStubs.pas',
  Test.BoldUMLTaggedValues in 'Code\Common\Test.BoldUMLTaggedValues.pas',
  Test.BoldIndexCollection in 'Code\Common\Test.BoldIndexCollection.pas',
  Test.BoldEventQueue in 'Code\Common\Test.BoldEventQueue.pas',
  Test.BoldTaggedValueList in 'Code\Common\Test.BoldTaggedValueList.pas',
  Test.BoldSharedStrings in 'Code\Common\Test.BoldSharedStrings.pas',
  Test.BoldBase in 'Code\Common\Test.BoldBase.pas',
  Test.BoldNamedValueList in 'Code\Common\Test.BoldNamedValueList.pas',
  Test.BoldHashIndexes in 'Code\Common\Test.BoldHashIndexes.pas',
  Test.BoldDeriver in 'Code\Common\Test.BoldDeriver.pas',
  Test.BoldSubscription in 'Code\Common\Test.BoldSubscription.pas',
  Test.BoldSubscribableCollection in 'Code\Common\Test.BoldSubscribableCollection.pas',
  Test.BoldBase64 in 'Code\Common\Test.BoldBase64.pas',
  Test.BoldSorter in 'Code\Common\Test.BoldSorter.pas',
  Test.BoldTaggedValueSupport in 'Code\Common\Test.BoldTaggedValueSupport.pas',
  Test.BoldNameExpander in 'Code\Common\Test.BoldNameExpander.pas',
  Test.BoldTypeNameDictionary in 'Code\Common\Test.BoldTypeNameDictionary.pas',
  Test.BoldGlobalId in 'Code\ValueSpace\Test.BoldGlobalId.pas',
  Test.BoldCondition in 'Code\ValueSpace\Test.BoldCondition.pas',
  Test.BoldDefaultXMLStreaming in 'Code\ValueSpace\Test.BoldDefaultXMLStreaming.pas',
  Test.BoldDefaultId in 'Code\ValueSpace\Test.BoldDefaultId.pas',
  Test.BoldId in 'Code\ValueSpace\Test.BoldId.pas',
  Test.BoldAttributes in 'Code\ObjectSpace\Test.BoldAttributes.pas',
  Test.BoldElements in 'Code\ObjectSpace\Test.BoldElements.pas',
  Test.BoldUMLModelValidator in 'Code\UMLModel\Test.BoldUMLModelValidator.pas',
  Test.BoldListHandle in 'Code\Handles\Test.BoldListHandle.pas',
  Test.BoldSortedHandle in 'Code\Handles\Test.BoldSortedHandle.pas',
  Test.BoldFilteredHandle in 'Code\Handles\Test.BoldFilteredHandle.pas',
  Test.BoldOclVariables in 'Code\Handles\Test.BoldOclVariables.pas',
  Test.BoldCollections in 'Code\Common\Test.BoldCollections.pas',
  Test.BoldSSExcept in 'Code\ObjectSpace\Test.BoldSSExcept.pas',
  Test.BoldDefs in 'Code\Common\Test.BoldDefs.pas',
  Test.BoldURI in 'Code\Common\Test.BoldURI.pas',
  Test.BoldLogReceiverInterface in 'Code\Common\Test.BoldLogReceiverInterface.pas',
  { Mock tests using Delphi-Mocks framework }
  Test.BoldDBInterfacesMock in 'Code\Mocks\Test.BoldDBInterfacesMock.pas',
  { Integration tests with transaction rollback }
  BoldTestPersistence in 'Code\Integration\BoldTestPersistence.pas',
  Test.BoldPersistence in 'Code\Integration\Test.BoldPersistence.pas',
  Test.BoldMeta in 'Code\MoldModel\Test.BoldMeta.pas';

{$R *.res}

begin
  RunTests;
end.
