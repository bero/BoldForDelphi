program InvoicingFMX;

{ The FMX client of the Invoicing example (#117 step 10): customers, their
  invoices and the invoice lines in three grids, through TBoldDataSet and
  LiveBindings. See README.md.

  Usage: InvoicingFMX
  The database InvoicingFMX.db next to the exe is created and filled with the
  scenario of InvoicingScenario on the first run.

  Usage: InvoicingFMX --selftest
  Shows no window. Fills a temporary database, checks that the grids show the
  customers, the invoices of each customer and the lines of each invoice,
  writes the checks to InvoicingFMX.selftest.txt next to the exe and exits
  with the number of failed checks. }

uses
  System.StartUpCopy,
  System.SysUtils,
  FMX.Forms,
  FireDAC.FMXUI.Wait,
  dmInvoicing in '..\Data\dmInvoicing.pas' {dmInvoicing: TDataModule},
  InvoicingClasses in '..\Model\InvoicingClasses.pas',
  InvoicingScenario in '..\Tests\InvoicingScenario.pas',
  fInvoicingMain in 'fInvoicingMain.pas' {frmInvoicingMain};

begin
  Application.Initialize;
  Application.CreateForm(TfrmInvoicingMain, frmInvoicingMain);
  if IsSelfTestRun then
  begin
    StartSelfTestLog;
    try
      // FMX creates the forms in Run; the self-test creates them without it
      Application.RealCreateForms;
      ExitCode := frmInvoicingMain.RunSelfTest;
    except
      // Raised outside FMX's message handling, so Application.OnException
      // never sees it: without this the RTL reports it and exits with 1
      on E: Exception do
      begin
        SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
        ExitCode := 100;
      end;
    end;
    // Application.Run registers the exit procedure that frees the forms before
    // the units are finalized. Without Run, FMX frees them in its own
    // finalization, after Bold's units, and the shutdown crashes; so now
    FreeAndNil(frmInvoicingMain);
  end
  else
    Application.Run;
end.
