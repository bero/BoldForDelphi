{ ============================================================================
  Step2.dpr - Bold Tutorial Step 2: Customer CRUD
  ============================================================================

  Builds on Step 1 by adding data-aware UI components:
    - TBoldListHandle  : Provides a list of objects via an OCL expression
    - TBoldGrid        : Displays Bold objects in a grid (like TDBGrid for Bold)
    - TBoldNavigator   : Add/delete/move buttons (like TDBNavigator for Bold)
    - TBoldEdit        : Edits a single attribute of the current object

  The InvoiceClasses unit MUST be listed here even though it's not directly
  referenced in code. Bold's initialization section registers the generated
  classes, so they're available when the system activates.
  ============================================================================ }

program Step2;

uses
  Vcl.Forms,
  Step2Form in 'Step2Form.pas' {frmStep2},
  InvoiceDataModule in '..\Shared\InvoiceDataModule.pas' {dmInvoice: TDataModule},
  InvoiceClasses in '..\Shared\InvoiceClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  // Data module first (Bold infrastructure), then the form
  Application.CreateForm(TInvoiceDataModule, dmInvoice);
  Application.CreateForm(TfrmStep2, frmStep2);
  Application.Run;
end.
