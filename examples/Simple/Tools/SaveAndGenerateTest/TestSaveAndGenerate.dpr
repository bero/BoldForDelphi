program TestSaveAndGenerate;

uses
  Vcl.Forms,
  TestMainForm in 'TestMainForm.pas' {frmTestMain},
  DemoDataModule in '..\..\..\Shared\DemoDataModule.pas' {dmDemo: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDemoDataModule, dmDemo);
  Application.CreateForm(TfrmTestMain, frmTestMain);
  Application.Run;
end.
