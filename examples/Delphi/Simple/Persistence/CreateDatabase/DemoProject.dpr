program DemoProject;

uses
  Vcl.Forms,
  DemoMainForm in 'DemoMainForm.pas' {frmDemoMain},
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoMain, frmDemoMain);
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.Run;
end.
