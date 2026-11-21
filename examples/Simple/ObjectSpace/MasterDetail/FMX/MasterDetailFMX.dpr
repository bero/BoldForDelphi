program MasterDetailFMX;

{ The Master-Detail demo in FireMonkey (#117 step 10): projects, the tasks of
  the selected project and all tasks, in three grids, through TBoldDataSet
  and LiveBindings. See README.md.

  Usage: MasterDetailFMX
  The database MasterDetailFMX.db next to the exe is created on the first run.

  Usage: MasterDetailFMX --selftest
  Shows no window. Works on a temporary database through the form's
  navigators, datasets and buttons, checks what the grids show, writes the
  checks to MasterDetailFMX.selftest.txt next to the exe and exits with the
  number of failed checks. }

uses
  {$IFDEF BOLD_STACKER}
  Stacker, // the optional stack-trace provider (#130), first so that its hooks are in place
  {$ENDIF}
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  FireDAC.FMXUI.Wait,
  DemoClasses in '..\..\..\..\Shared\DemoClasses.pas',
  dmMasterDetail in 'dmMasterDetail.pas' {dmMasterDetail: TDataModule},
  fMasterDetailMain in 'fMasterDetailMain.pas' {frmMasterDetailMain};

// The resource file the build makes from the project: on Windows it holds
// the manifest, whose dependency on the common controls version 6 the hints
// need (FMX shows them as native tooltips there)
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMasterDetailMain, frmMasterDetailMain);
  if IsSelfTestRun then
  begin
    StartSelfTestLog;
    try
      // FMX creates the forms in Run; the self-test creates them without it
      Application.RealCreateForms;
      ExitCode := frmMasterDetailMain.RunSelfTest;
    except
      // Raised outside FMX's message handling, so Application.OnException
      // never sees it: without this the RTL reports it and exits with 1
      on E: Exception do
      begin
        SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
        ExitCode := 100;
      end;
    end;
    // Application.Run registers the exit procedure that frees the forms
    // before the units are finalized. Without Run, FMX frees them in its own
    // finalization, after Bold's units, and the shutdown crashes; so now
    FreeAndNil(frmMasterDetailMain);
  end
  else
    Application.Run;
end.
