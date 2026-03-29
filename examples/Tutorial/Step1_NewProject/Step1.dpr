{ ============================================================================
  Step1.dpr - Bold Tutorial Step 1: New Project Setup
  ============================================================================

  This is the simplest possible Bold application. It demonstrates:
    - Creating a BoldModel (your UML class diagram at runtime)
    - Creating a BoldSystemHandle (the in-memory object space)
    - Connecting to a SQLite database for persistence
    - Opening and closing the Bold system

  CREATION ORDER MATTERS
  ----------------------
  The data module (TInvoiceDataModule) must be created BEFORE the main form.
  This is because the form's FormCreate handler calls dmInvoice.OpenSystem,
  which requires the data module to already exist. Delphi's CreateForm calls
  happen in the order listed below.

  UNITS IN THE USES CLAUSE
  ------------------------
  - Step1Form           : The main form (UI)
  - InvoiceDataModule   : The Bold infrastructure (model + persistence)
  - InvoiceClasses      : Generated business classes (Customer, Invoice, etc.)
                          Must be in the uses clause so Bold can find and
                          register them at startup via the initialization section.
  ============================================================================ }

program Step1;

uses
  Vcl.Forms,
  Step1Form in 'Step1Form.pas' {frmStep1},
  InvoiceDataModule in '..\Shared\InvoiceDataModule.pas' {dmInvoice: TDataModule},
  InvoiceClasses in '..\Shared\InvoiceClasses.pas';

{$R *.RES}

begin
  // Report any memory leaks when the application shuts down.
  // Useful during development to catch objects that weren't freed.
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Create the data module first - it sets up Bold infrastructure
  Application.CreateForm(TInvoiceDataModule, dmInvoice);
  // Then create the main form - its FormCreate will call dmInvoice.OpenSystem
  Application.CreateForm(TfrmStep1, frmStep1);

  Application.Run;
end.
