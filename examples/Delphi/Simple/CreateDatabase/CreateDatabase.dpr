program CreateDatabase;

uses
  Vcl.Forms,
  DemoMainForm in 'DemoMainForm.pas' {frmDemoMain},
  DemoDataModule in '..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule},
  DemoClasses in '..\..\Shared\DemoClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.CreateForm(TfrmDemoMain, frmDemoMain);
  Application.Run;
end.
