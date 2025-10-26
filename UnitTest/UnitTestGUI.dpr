program UnitTestGUI;

{********************************
 *                              *
 * Unittests from GUI           *
 * Useful when change unittests *
 *                              *
 ********************************}

{$APPTYPE GUI}
{$STRONGLINKTYPES ON}
{$R *.res}

uses
  FastMM4 in '..\Source\Extensions\FastMM4\FastMM4.pas',
  FastMM4Messages in '..\Source\Extensions\FastMM4\FastMM4Messages.pas',
  DUnitX.MemoryLeakMonitor.FastMM4 in '..\..\DUnitX\Source\DUnitX.MemoryLeakMonitor.FastMM4.pas',
  System.SysUtils,
  DUnitX.Loggers.GUI.VCL,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  VCL.Forms,
  jano_multiplicity in 'jano_multiplicity.pas' {keep comment here to protect the following conditional from being removed by the IDE when adding a unit},
  BoldUMLTypes in '..\Source\Common\UML\BoldUMLTypes.pas',
  BoldBase in '..\Source\Common\Core\BoldBase.pas',
  BoldDefs in '..\Source\Common\Core\BoldDefs.pas',
  BoldCommonConst in '..\Source\Common\Core\BoldCommonConst.pas',
  BoldIndexableList in '..\Source\Common\Support\BoldIndexableList.pas',
  BoldIndex in '..\Source\Common\Support\BoldIndex.pas',
  BoldHashIndexes in '..\Source\Common\Support\BoldHashIndexes.pas',
  BoldUtils in '..\Source\Common\Support\BoldUtils.pas',
  BoldRev in '..\Source\Common\Support\BoldRev.pas',
  BoldContainers in '..\Source\Common\Core\BoldContainers.pas',
  TestStartup in 'TestStartup.pas';

begin
  Application.Initialize;
  Application.Title := 'DUnitX';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  TestStartup.Run;
end.

