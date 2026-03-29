{ ============================================================================
  Step4.dpr - Bold Tutorial Step 4: OCL Expressions
  ============================================================================

  The final step adds powerful OCL (Object Constraint Language) features:
    - TBoldLabel with OCL expressions for computed values
      (customer count, invoice count, outstanding totals)
    - Filtered lists using OCL ->select() expressions
      (e.g., Invoice.allInstances->select(not isPaid))
    - Derived attributes that Bold auto-calculates
      (TotalAmount = items.lineTotal->sum)

  OCL replaces SQL for querying in Bold. Instead of writing
  "SELECT COUNT(*) FROM Customer", you write "Customer.allInstances->size".
  Bold evaluates OCL against in-memory objects AND can translate it to SQL
  for database-level queries.
  ============================================================================ }

program Step4;

uses
  Vcl.Forms,
  Step4Form in 'Step4Form.pas' {frmStep4},
  InvoiceDataModule in '..\Shared\InvoiceDataModule.pas' {dmInvoice: TDataModule},
  InvoiceClasses in '..\Shared\InvoiceClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TInvoiceDataModule, dmInvoice);
  Application.CreateForm(TfrmStep4, frmStep4);
  Application.Run;
end.
