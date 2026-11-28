{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program ModelEditorLCL;

{ The LCL client of the standalone model editor (#128): open a Bold model
  (.bld), edit its classes and their attributes, save it and generate the
  business classes. See ../README.md.

  Usage: ModelEditorLCL [--typenames=form] [model.bld]
  Opens the model file when one is named. --typenames: generate code with
  the type names of the TBoldTypeNameHandle on that form (.dfm, .lfm, .fmx),
  as File > Type names from form does.

  Usage: ModelEditorLCL --selftest [--typenames=form] model.bld
  Shows no window. Opens the model, checks that the grids show the classes
  and the attributes of the selected class, that the commands add, delete,
  save and generate and that the consistency check lists a finding that
  selects its class, writes the checks to ModelEditorLCL.selftest.txt
  next to the program and exits with the number of failed checks. The
  model file itself is not changed: the self-test saves and generates into a
  temporary folder. }

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces,
  BoldExceptionLogLCL, // exceptions to ModelEditorLCL.log with their call stacks (#130)
  SysUtils,
  Forms,
  fModelEditorMain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  if IsSelfTestRun then
  begin
    StartSelfTestLog;
    try
      // CreateForm creates the form at once; without Run it is never shown
      Application.CreateForm(TfrmModelEditorMain, frmModelEditorMain);
      ExitCode := frmModelEditorMain.RunSelfTest;
    except
      on E: Exception do
      begin
        SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
        ExitCode := 100;
      end;
    end;
    // Before Bold's units are finalized
    FreeAndNil(frmModelEditorMain);
  end
  else
  begin
    Application.Title := 'Bold Model Editor';
    Application.CreateForm(TfrmModelEditorMain, frmModelEditorMain);
    Application.Run;
  end;
end.
