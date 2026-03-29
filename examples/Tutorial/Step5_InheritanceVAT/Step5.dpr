{ ============================================================================
  Step5.dpr - Bold Tutorial Step 5: Inheritance & VAT
  ============================================================================

  This step extends the Invoice model with two key OOP concepts:

    1. INHERITANCE - Customer becomes a base class with two subclasses:
       - TCompany     (has VATNumber, VATExempt)
       - TPrivatePerson (no extra fields)

       In Bold, inheritance maps to a single database table (Table-Per-Hierarchy
       strategy). Bold stores a type column (BOLD_TYPE) to distinguish rows.
       You create objects of the concrete subclass, but can query all customers
       via Customer.allInstances (which returns both companies and persons).

    2. LOOKUP/REFERENCE CLASSES - TVATRate is a simple reference table:
       - Name (e.g. "Standard 24%", "Reduced 14%", "Zero")
       - Percentage (e.g. 24.0, 14.0, 0.0)

       InvoiceItem has a link to VATRate, and a derived VATAmount attribute
       that Bold calculates automatically from lineTotal and the rate.

  The form demonstrates:
    - Creating Company vs PrivatePerson objects (two separate buttons)
    - Company-specific fields (VATNumber, VATExempt) that only appear for companies
    - Managing VAT rates as a lookup table
    - Derived VATAmount on invoice items using OCL with association navigation
  ============================================================================ }

program Step5;

uses
  Vcl.Forms,
  Step5Form in 'Step5Form.pas' {frmStep5},
  Step5DataModule in 'Step5DataModule.pas' {dmStep5: TDataModule},
  InvoiceClasses in 'InvoiceClasses.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TStep5DataModule, dmStep5);
  Application.CreateForm(TfrmStep5, frmStep5);
  Application.Run;
end.
