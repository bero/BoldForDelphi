program CreateDatabase;

uses
  Vcl.Forms,
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule},
  DemoMainForm in 'DemoMainForm.pas' {frmDemoMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoMain, frmDemoMain);
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.Run;
end.
