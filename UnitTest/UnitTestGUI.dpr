program UnitTestGUI;

{$STRONGLINKTYPES ON}

uses
  Forms,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.TestFramework,
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
  maan_Modify in 'Code\ObjectSpace\maan_Modify.pas',
  maan_Undo in 'Code\ObjectSpace\maan_Undo.pas',
  Test.BoldSystem in 'Code\ObjectSpace\Test.BoldSystem.pas',
  Test.BoldLinks in 'Code\ObjectSpace\Test.BoldLinks.pas',
  Test.BoldFreeStandingValueFactories in 'Code\FreestandingValueSpace\Test.BoldFreeStandingValueFactories.pas',
  Test.BoldDefaultTaggedValues in 'Code\Common\Test.BoldDefaultTaggedValues.pas',
  Test.BoldMemberTypeDictionary in 'Code\RTModel\Test.BoldMemberTypeDictionary.pas',
  Test.BoldGeneratedCodeDictionary in 'Code\RTModel\Test.BoldGeneratedCodeDictionary.pas',
  Test.BoldPersistenceHandleDB in 'Code\Persistence\Test.BoldPersistenceHandleDB.pas',
  Test.BoldPMapperLists in 'Code\PMapper\Test.BoldPMapperLists.pas',
  Test.BoldSQLMappingInfo in 'Code\PMapper\Test.BoldSQLMappingInfo.pas',
  Test.BoldPMappersDefault in 'Code\PMapper\Test.BoldPMappersDefault.pas',
  Test.BoldUtils in 'Code\Common\Test.BoldUtils.pas',
  Test.PersistenceFireDAC in 'Code\Persistence\Test.PersistenceFireDAC.pas',
  Test.BoldGUIDUtils in 'Code\Common\Test.BoldGUIDUtils.pas',
  Test.BoldMD5 in 'Code\ProductControl\Test.BoldMD5.pas',
  Test.BoldThreadSafeQueue in 'Code\Propagator\Test.BoldThreadSafeQueue.pas',
  Test.BoldThreadSafeLog in 'Code\Common\Test.BoldThreadSafeLog.pas',
  Test.BoldLogInterfaces in 'Code\Common\Test.BoldLogInterfaces.pas',
  Test.BoldAttributes in 'Code\ObjectSpace\Test.BoldAttributes.pas',
  Test.BoldUMLModelValidator in 'Code\UMLModel\Test.BoldUMLModelValidator.pas',
  Test.BoldListHandle in 'Code\Handles\Test.BoldListHandle.pas',
  { Mock tests using Delphi-Mocks framework }
  Test.BoldDBInterfacesMock in 'Code\Mocks\Test.BoldDBInterfacesMock.pas',
  { Integration tests with transaction rollback }
  BoldTestPersistence in 'Code\Integration\BoldTestPersistence.pas',
  Test.BoldPersistence in 'Code\Integration\Test.BoldPersistence.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Title := 'Bold for Delphi Unit Tests';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
