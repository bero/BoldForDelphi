program OclWorkbench;

{-----------------------------------------------------------------------------
  A bench for writing OCL against a live Bold system.

  The data module has to be created before the main form, because the form's
  handles are wired in the designer to DemoDataModule.BoldSystemHandle1 and the
  DFM loader resolves that by name at load time.
-----------------------------------------------------------------------------}

uses
  Vcl.Forms,
  OclWorkbenchForm in 'OclWorkbenchForm.pas' {frmOclWorkbench},
  OclWorkbenchSamples in 'OclWorkbenchSamples.pas',
  OclWorkbenchEngines in 'OclWorkbenchEngines.pas',
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule},
  DemoClasses in '..\..\..\Shared\DemoClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Bold OCL Workbench';
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.CreateForm(TfrmOclWorkbench, frmOclWorkbench);
  Application.Run;
end.
