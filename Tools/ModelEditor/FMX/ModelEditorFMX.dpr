program ModelEditorFMX;

{ The FMX client of the standalone model editor (#128): open a Bold model
  (.bld), edit its classes and their attributes, save it and generate the
  business classes. The FireMonkey twin of ../LCL/ModelEditorLCL.lpr; both
  are built on the data module in ../Core. See ../README.md.

  Usage: ModelEditorFMX [--typenames=form] [model.bld]
  Opens the model file when one is named. --typenames: generate code with
  the type names of the TBoldTypeNameHandle on that form (.dfm, .lfm, .fmx),
  as File > Type names from form does.

  Usage: ModelEditorFMX --selftest --typenames=form model.bld
  The self-test expects the form of the CustomAttributes example
  (examples/Simple/ObjectSpace/CustomAttributes/MainForm.dfm) and the
  Invoicing model. Shows no window. Opens the model, checks that the grids show the classes
  and the attributes of the selected class, that the commands add, delete,
  save and generate and that the consistency check lists a finding that
  selects its class, writes the checks to ModelEditorFMX.selftest.txt
  next to the program and exits with the number of failed checks. The model
  file itself is not changed: the self-test saves and generates into a
  temporary folder. }

uses
  {$IFDEF BOLD_STACKER}
  Stacker, // the optional stack-trace provider (#130), first so that its hooks are in place
  {$ENDIF}
  System.StartUpCopy,
  BoldExceptionLogFMX, // exceptions to ModelEditorFMX.log with their call stacks (#130)
  System.SysUtils,
  FMX.Forms,
  BoldModelDocument in '..\Core\BoldModelDocument.pas',
  BoldModelEditorData in '..\Core\BoldModelEditorData.pas' {dmModelEditor: TDataModule},
  fModelEditorMain in 'fModelEditorMain.pas' {frmModelEditorMain},
  fModelEditorClass in 'fModelEditorClass.pas' {frmModelEditorClass},
  fModelEditorAttribute in 'fModelEditorAttribute.pas' {frmModelEditorAttribute};

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
