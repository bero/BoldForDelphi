program UnitTestGUI;

{$STRONGLINKTYPES ON}

uses
  Forms,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.TestFramework,
  BoldTestCase in 'BoldTestCase.pas',
  BoldTestCaseDB in 'BoldTestCaseDB.pas',
  Test.BoldUMLTypes in 'Code\Common\Test.BoldUMLTypes.pas',
  Test.BoldAttributes in 'Code\ObjectSpace\Test.BoldAttributes.pas',
  jehoBCBoldTest in 'Code\ObjectSpace\jehoBCBoldTest.pas',
  Test.FetchInvalidAttribute in 'Code\ObjectSpace\Test.FetchInvalidAttribute.pas',
  TestModel1 in 'Code\Main\TestModel1.pas',
  UndoTestModelClasses in 'Code\ObjectSpace\UndoTestModelClasses.pas',
  maan_UndoRedoTestCaseUtils in 'Code\ObjectSpace\maan_UndoRedoTestCaseUtils.pas',
  maan_UndoRedo in 'Code\ObjectSpace\maan_UndoRedo.pas',
  Test.BoldSystem in 'Code\ObjectSpace\Test.BoldSystem.pas',
  Test.BoldFreeStandingValueFactories in 'Code\FreestandingValueSpace\Test.BoldFreeStandingValueFactories.pas',
  Test.BoldDefaultTaggedValues in 'Code\Common\Test.BoldDefaultTaggedValues.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Title := 'Bold for Delphi Unit Tests';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end.
