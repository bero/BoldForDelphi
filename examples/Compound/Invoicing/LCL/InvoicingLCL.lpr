{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
program InvoicingLCL;

{ The LCL client of the Invoicing example (#117 step 17): customers, their
  invoices and the invoice lines in three TDBGrids, through TBoldDataSet.
  See README.md.

  Usage: InvoicingLCL
  The data file InvoicingLCL.xml next to the program is created and filled
  with the scenario of InvoicingScenario on the first run.

  Usage: InvoicingLCL --selftest
  Shows no window. Fills a temporary data file, checks that the grids are
  bound to the customers, the invoices of each customer and the lines of each
  invoice, writes the checks to InvoicingLCL.selftest.txt next to the program
  and exits with the number of failed checks. }

uses
  {$IFDEF UNIX}cthreads, cwstring,{$ENDIF}
  Interfaces,
  SysUtils,
  Forms,
  fInvoicingMain;

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  if IsSelfTestRun then
  begin
    StartSelfTestLog;
    try
      // CreateForm creates the form at once; without Run it is never shown
      Application.CreateForm(TfrmInvoicingMain, frmInvoicingMain);
      ExitCode := frmInvoicingMain.RunSelfTest;
    except
      on E: Exception do
      begin
        SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
        ExitCode := 100;
      end;
    end;
    // Before Bold's units are finalized
    FreeAndNil(frmInvoicingMain);
  end
  else
  begin
    Application.CreateForm(TfrmInvoicingMain, frmInvoicingMain);
    Application.Run;
  end;
end.
