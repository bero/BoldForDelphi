program ModelEditorFMX;

{ The FMX client of the standalone model editor (#128): open a Bold model
  (.bld), edit its classes and their attributes, save it and generate the
  business classes. The FireMonkey twin of ../LCL/ModelEditorLCL.lpr; both
  are built on the data module in ../Core. See ../README.md.

  Usage: ModelEditorFMX [model.bld]
  Opens the model file when one is named.

  Usage: ModelEditorFMX --selftest model.bld
  Shows no window. Opens the model, checks that the grids show the classes
  and the attributes of the selected class and that the commands add,
  delete, save and generate, writes the checks to ModelEditorFMX.selftest.txt
  next to the program and exits with the number of failed checks. The model
  file itself is not changed: the self-test saves and generates into a
  temporary folder. }

uses
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  BoldModelDocument in '..\Core\BoldModelDocument.pas',
  BoldModelEditorData in '..\Core\BoldModelEditorData.pas' {dmModelEditor: TDataModule},
  fModelEditorMain in 'fModelEditorMain.pas' {frmModelEditorMain};

// The resource file the build makes from the project: on Windows it holds
// the manifest, whose dependency on the common controls version 6 the hints
// need (FMX shows them as native tooltips there)
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmModelEditorMain, frmModelEditorMain);
  if IsSelfTestRun then
  begin
    StartSelfTestLog;
    try
      // FMX creates the forms in Run; the self-test creates them without it
      Application.RealCreateForms;
      ExitCode := frmModelEditorMain.RunSelfTest;
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
    FreeAndNil(frmModelEditorMain);
  end
  else
    Application.Run;
end.
