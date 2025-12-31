program MasterDetail;

uses
  Vcl.Forms,
  DemoForm in 'DemoForm.pas' {MainForm},
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule},
  DemoClasses in '..\..\..\Shared\DemoClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
