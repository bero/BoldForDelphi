{ ============================================================================
  Step3.dpr - Bold Tutorial Step 3: Invoices and Associations
  ============================================================================

  Builds on Step 2 by introducing:
    - UML associations (Customer -> Invoices -> Items)
    - Master-detail binding via handle chaining
    - Three-panel layout showing related objects

  Handle chaining is the key concept here:
    lhaCustomers (root)  -->  lhaInvoices (rooted on lhaCustomers)
                                  |
                                  v
                              lhaItems (rooted on lhaInvoices)

  When you select a customer, lhaInvoices automatically shows only that
  customer's invoices. When you select an invoice, lhaItems shows only
  that invoice's items. No code needed - Bold handles this reactively.
  ============================================================================ }

program Step3;

uses
  Vcl.Forms,
  Step3Form in 'Step3Form.pas' {frmStep3},
  InvoiceDataModule in '..\Shared\InvoiceDataModule.pas' {dmInvoice: TDataModule},
  InvoiceClasses in '..\Shared\InvoiceClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TInvoiceDataModule, dmInvoice);
  Application.CreateForm(TfrmStep3, frmStep3);
  Application.Run;
end.
