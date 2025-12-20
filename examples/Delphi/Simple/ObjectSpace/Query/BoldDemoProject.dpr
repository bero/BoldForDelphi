program BoldDemoProject;

uses
  Vcl.Forms,
  DemoForm in 'DemoForm.pas' {frmQueryDemo},
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDemoDataModule},
  DemoClasses in '..\..\..\Shared\DemoClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.CreateForm(TfrmQueryDemo, frmQueryDemo);
  Application.Run;
end.
