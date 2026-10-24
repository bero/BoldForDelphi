unit InvoicingScenario;

{ The acceptance scenario of the Invoicing example, written as plain procedures
  over a TBoldSystem.

  The unit knows nothing about the database, the GUI or a test framework: the
  caller opens the system and supplies a check callback. That is what lets the
  same scenario run from the console runner, from the DUnitX tests and, once
  Bold compiles there, from a Lazarus runner. The expected values are the
  acceptance numbers every platform runner has to reproduce. }

interface

uses
  SysUtils,
  BoldDefs,
  BoldElements,
  BoldAttributes,
  BoldSystem,
  InvoicingClasses;

type
  { Called once per check: what was checked, whether it passed and the
    observed value as text for the report. }
  TCheckProc = procedure(const AWhat: string; APassed: Boolean; const AActual: string);

{ Creating the abstract Customer class must fail. }
procedure CheckAbstractCustomerRefused(ASystem: TBoldSystem; ACheck: TCheckProc);

{ Builds the company, a VAT rate, a product, two customers and one invoice
  through the business methods, and checks every derived attribute, every
  constraint and every refusal on the way. Leaves the objects unsaved. }
procedure BuildAndVerifyScenario(ASystem: TBoldSystem; ACheck: TCheckProc);

{ Checks the facts that must survive a save, a close and a reopen. }
procedure VerifyReloadedScenario(ASystem: TBoldSystem; ACheck: TCheckProc);

function Money(AValue: Currency): string;

implementation

const
  cBillingAddressText = 'Teollisuuskatu 5, 00510 Helsinki, Finland';
  cFirstInvoiceNumber = 1001;

function Money(AValue: Currency): string;
begin
  Result := FormatCurr('0.00', AValue);
end;

function Bool(AValue: Boolean): string;
begin
  Result := BoolToStr(AValue, True);
end;

function CountOf(ASystem: TBoldSystem; const AExpression: string): Integer;
begin
  Result := ASystem.EvaluateExpressionAsInteger(AExpression);
end;

procedure FillAddress(AAddress: TAddress; const AStreet, APostalCode, ACity, ACountry: string);
begin
  AAddress.Street := AStreet;
  AAddress.PostalCode := APostalCode;
  AAddress.City := ACity;
  AAddress.Country := ACountry;
end;

{ Lists every constraint Bold knows for the element: the ones from the model
  plus the multiplicity constraints Bold derives from required roles. }
procedure CheckConstraints(AElement: TBoldElement; const AWhat: string; AExpectedCount: Integer; ACheck: TCheckProc);
var
  List: TBoldList;
  Constraint: TBAConstraint;
  i: Integer;
begin
  List := AElement.EvaluateExpressionAsNewElement('constraints') as TBoldList;
  try
    ACheck(AWhat + ' constraint count', List.Count = AExpectedCount, IntToStr(List.Count));
    for i := 0 to List.Count - 1 do
    begin
      Constraint := List[i] as TBAConstraint;
      ACheck(AWhat + ': ' + Constraint.Constraint.ConstraintExpression, Constraint.AsBoolean, Bool(Constraint.AsBoolean));
    end;
  finally
    List.Free;
  end;
end;

procedure CheckAbstractCustomerRefused(ASystem: TBoldSystem; ACheck: TCheckProc);
begin
  try
    TCustomer.Create(ASystem);
    ACheck('TCustomer.Create refused', False, 'an instance was created');
  except
    on E: EBold do
      ACheck('TCustomer.Create refused', True, E.Message);
  end;
end;

procedure BuildAndVerifyScenario(ASystem: TBoldSystem; ACheck: TCheckProc);
var
  Settings: TCompanySettings;
  Vat: TVatRate;
  Product: TProduct;
  Company: TCompany;
  Contact: TContactPerson;
  Person: TPrivatePerson;
  Invoice: TInvoice;
  Draft: TInvoice;
  Line: TInvoiceLine;
  Payment: TPayment;
begin
  // Company settings: one per database, created on first use.
  Settings := TCompanySettings.Instance(ASystem);
  ACheck('Settings: Instance creates the single object', CountOf(ASystem, 'CompanySettings.allInstances->size') = 1,
    IntToStr(CountOf(ASystem, 'CompanySettings.allInstances->size')));
  ACheck('Settings: Instance returns the same object again', TCompanySettings.Instance(ASystem) = Settings, Bool(TCompanySettings.Instance(ASystem) = Settings));
  ACheck('Settings: address exists from birth', Assigned(Settings.Address), Bool(Assigned(Settings.Address)));
  ACheck('Settings: NextCustomerNumber starts at 1', Settings.NextCustomerNumber = 1, IntToStr(Settings.NextCustomerNumber));
  ACheck('Settings: DefaultPaymentTermDays starts at 14', Settings.DefaultPaymentTermDays = 14, IntToStr(Settings.DefaultPaymentTermDays));
  Settings.CompanyName := 'Bold Consulting Oy';
  Settings.Email := 'invoices@boldconsulting.example';
  Settings.Phone := '+358 9 123 4567';
  Settings.VatNumber := 'FI01234567';
  Settings.BankAccount := 'FI21 1234 5600 0007 85';
  Settings.NextInvoiceNumber := cFirstInvoiceNumber;
  FillAddress(Settings.Address, 'Mannerheimintie 1', '00100', 'Helsinki', 'Finland');

  Vat := TVatRate.Create(ASystem);
  Vat.Name := 'Standard 25.5 %';
  Vat.Percent := 25.5;
  Vat.IsDefault := True;

  Product := TProduct.Create(ASystem);
  Product.Code := 'CONS-H';
  Product.Name := 'Consulting hour';
  Product.UnitOfMeasure := 'h';
  Product.UnitPrice := 120;
  Product.IsActive := True;
  Product.VatRate := Vat;

  // Customers: numbered and given a billing address by CompleteCreate.
  Company := TCompany.Create(ASystem);
  ACheck('Company: Number allocated on creation', Company.Number = 1, IntToStr(Company.Number));
  ACheck('Company: active from birth', Company.IsActive, Bool(Company.IsActive));
  ACheck('Company: billing address exists from birth', Assigned(Company.BillingAddress), Bool(Assigned(Company.BillingAddress)));
  Company.Name := 'Acme Oy';
  Company.Email := 'ap@acme.example';
  Company.Phone := '+358 40 111 2222';
  Company.VatNumber := 'FI76543210';
  Company.BusinessId := '7654321-0';
  Company.PaymentTermDays := 14;
  FillAddress(Company.BillingAddress, 'Teollisuuskatu 5', '00510', 'Helsinki', 'Finland');

  Contact := TContactPerson.Create(ASystem);
  Contact.Name := 'Anna Virtanen';
  Contact.Title := 'Accounts payable';
  Contact.Email := 'anna.virtanen@acme.example';
  Contact.Company := Company;

  Person := TPrivatePerson.Create(ASystem);
  ACheck('Person: Number allocated on creation', Person.Number = 2, IntToStr(Person.Number));
  Person.Name := 'Erik Eriksson';
  Person.Email := 'erik@example.com';
  Person.PaymentTermDays := 30;
  FillAddress(Person.BillingAddress, 'Storgatan 12', '111 22', 'Stockholm', 'Sweden');
  ACheck('Settings: NextCustomerNumber advanced to 3', Settings.NextCustomerNumber = 3, IntToStr(Settings.NextCustomerNumber));

  ACheck('Company.AsString', Company.AsString = 'Acme Oy', Company.AsString);
  ACheck('Product.AsString', Product.AsString = 'CONS-H Consulting hour', Product.AsString);
  ACheck('Company.BillingAddress.AsText', Company.BillingAddress.AsText = cBillingAddressText, Company.BillingAddress.AsText);
  ACheck('Company.Contacts.Count', Company.Contacts.Count = 1, IntToStr(Company.Contacts.Count));

  // NewInvoice: customer, dates from the payment term, own copy of the address.
  Invoice := Company.NewInvoice;
  ACheck('NewInvoice: Customer', Invoice.Customer = Company, Invoice.Customer.AsString);
  ACheck('NewInvoice: InvoiceDate is today', Invoice.InvoiceDate = Date, DateToStr(Invoice.InvoiceDate));
  ACheck('NewInvoice: DueDate is today + 14', Invoice.DueDate = Date + 14, DateToStr(Invoice.DueDate));
  ACheck('NewInvoice: billing address copied', Invoice.BillingAddress.AsText = cBillingAddressText, Invoice.BillingAddress.AsText);
  ACheck('NewInvoice: billing address is a separate object', Invoice.BillingAddress <> Company.BillingAddress,
    Bool(Invoice.BillingAddress <> Company.BillingAddress));
  ACheck('NewInvoice: Status', Invoice.Status = 'Draft', Invoice.Status);
  // Back-date the invoice so the overdue step later keeps dueDate >= invoiceDate.
  Invoice.InvoiceDate := Date - 10;
  Invoice.DueDate := Invoice.InvoiceDate + Company.PaymentTermDays;
  Invoice.OurReference := 'Project Phoenix';

  // AddLine: snapshots of the product, immune to later price changes.
  Line := Invoice.AddLine(Product, 7.5);
  ACheck('AddLine: Description from product', Line.Description = 'Consulting hour', Line.Description);
  ACheck('AddLine: UnitPrice from product', Line.UnitPrice = 120, Money(Line.UnitPrice));
  ACheck('AddLine: VatPercent from product VAT rate', Line.VatPercent = 25.5, Money(Line.VatPercent));
  ACheck('AddLine: Product linked', Line.Product = Product, Line.Product.AsString);
  ACheck('Line.NetAmount', Line.NetAmount = 900, Money(Line.NetAmount));
  ACheck('Line.VatAmount', Line.VatAmount = 229.5, Money(Line.VatAmount));
  ACheck('Line.GrossAmount', Line.GrossAmount = 1129.5, Money(Line.GrossAmount));
  Product.UnitPrice := 150;
  ACheck('AddLine: price survives a product price change', Line.UnitPrice = 120, Money(Line.UnitPrice));
  Product.UnitPrice := 120;

  Line := Invoice.AddLine(nil, 1);
  Line.Description := 'Travel expenses';
  Line.UnitPrice := 49.90;
  Line.VatPercent := Vat.Percent;
  ACheck('Line.VatAmount rounded to cents', Line.VatAmount = 12.72, Money(Line.VatAmount));

  ACheck('Draft: Lines.Count', Invoice.Lines.Count = 2, IntToStr(Invoice.Lines.Count));
  ACheck('Draft: NetTotal', Invoice.NetTotal = 949.90, Money(Invoice.NetTotal));
  ACheck('Draft: VatTotal', Invoice.VatTotal = 242.22, Money(Invoice.VatTotal));
  ACheck('Draft: GrossTotal', Invoice.GrossTotal = 1192.12, Money(Invoice.GrossTotal));
  ACheck('Draft: Status', Invoice.Status = 'Draft', Invoice.Status);
  ACheck('Draft: AsString', Invoice.AsString = 'Draft', Invoice.AsString);
  ACheck('Draft: IsBusinessInvoice', Invoice.IsBusinessInvoice, Bool(Invoice.IsBusinessInvoice));
  ACheck('Draft: Company.InvoicedTotal excludes drafts', Company.InvoicedTotal = 0, Money(Company.InvoicedTotal));
  ACheck('Draft: may be deleted', Invoice.CanDelete, Bool(Invoice.CanDelete));

  try
    Invoice.AddPayment(100, Date);
    ACheck('Draft: AddPayment refused', False, 'a payment was added');
  except
    on E: EBold do
      ACheck('Draft: AddPayment refused', True, E.Message);
  end;

  // Posting an empty invoice is refused and consumes no number.
  Draft := Person.NewInvoice;
  ACheck('Person draft: DueDate is today + 30', Draft.DueDate = Date + 30, DateToStr(Draft.DueDate));
  try
    Draft.Post;
    ACheck('Empty invoice: Post refused', False, 'posted as ' + Draft.AsString);
  except
    on E: EBold do
      ACheck('Empty invoice: Post refused', True, E.Message);
  end;
  ACheck('Empty invoice: no number consumed', Settings.NextInvoiceNumber = cFirstInvoiceNumber, IntToStr(Settings.NextInvoiceNumber));
  Draft.Delete;

  // A customer without own payment term gets the company default.
  Person.PaymentTermDays := 0;
  Draft := Person.NewInvoice;
  ACheck('Person draft: DueDate falls back to the default term', Draft.DueDate = Date + 14, DateToStr(Draft.DueDate));
  Draft.Delete;
  Person.PaymentTermDays := 30;
  ACheck('Person: drafts deleted', Person.Invoices.Count = 0, IntToStr(Person.Invoices.Count));

  // An inactive customer cannot be invoiced.
  Person.IsActive := False;
  try
    Person.NewInvoice;
    ACheck('Inactive customer: NewInvoice refused', False, 'an invoice was created');
  except
    on E: EBold do
      ACheck('Inactive customer: NewInvoice refused', True, E.Message);
  end;
  Person.IsActive := True;

  // Post: number from the settings, invoice frozen.
  Invoice.Post;
  ACheck('Post: Number allocated', Invoice.Number = cFirstInvoiceNumber, IntToStr(Invoice.Number));
  ACheck('Post: NextInvoiceNumber advanced', Settings.NextInvoiceNumber = cFirstInvoiceNumber + 1, IntToStr(Settings.NextInvoiceNumber));
  ACheck('Posted: Status', Invoice.Status = 'Open', Invoice.Status);
  ACheck('Posted: AsString', Invoice.AsString = '1001', Invoice.AsString);
  ACheck('Posted: Balance', Invoice.Balance = 1192.12, Money(Invoice.Balance));
  ACheck('Posted: Company.InvoicedTotal', Company.InvoicedTotal = 1192.12, Money(Company.InvoicedTotal));
  ACheck('Posted: Company.OpenBalance', Company.OpenBalance = 1192.12, Money(Company.OpenBalance));

  try
    Invoice.Post;
    ACheck('Posted: Post again refused', False, 'posted twice');
  except
    on E: EBold do
      ACheck('Posted: Post again refused', True, E.Message);
  end;
  ACheck('Posted: Number unchanged', Invoice.Number = cFirstInvoiceNumber, IntToStr(Invoice.Number));

  try
    Invoice.AddLine(Product, 1);
    ACheck('Posted: AddLine refused', False, 'a line was added');
  except
    on E: EBold do
      ACheck('Posted: AddLine refused', True, E.Message);
  end;
  ACheck('Posted: Lines.Count unchanged', Invoice.Lines.Count = 2, IntToStr(Invoice.Lines.Count));

  ACheck('Posted: may not be deleted', not Invoice.CanDelete, Bool(Invoice.CanDelete));
  try
    Invoice.Delete;
  except
    on E: EBold do
      ACheck('Posted: Delete raised', True, E.Message);
  end;
  ACheck('Posted: still exists after Delete', CountOf(ASystem, 'Invoice.allInstances->size') = 1,
    IntToStr(CountOf(ASystem, 'Invoice.allInstances->size')));

  // Payments.
  Payment := Invoice.AddPayment(500, Date);
  Payment.Reference := 'Partial';
  ACheck('Partial: PaidAmount', Invoice.PaidAmount = 500, Money(Invoice.PaidAmount));
  ACheck('Partial: Balance', Invoice.Balance = 692.12, Money(Invoice.Balance));
  ACheck('Partial: IsPaid', not Invoice.IsPaid, Bool(Invoice.IsPaid));

  Invoice.DueDate := Date - 1;
  ACheck('Overdue: IsOverdue', Invoice.IsOverdue, Bool(Invoice.IsOverdue));
  ACheck('Overdue: Status', Invoice.Status = 'Overdue', Invoice.Status);
  ACheck('Overdue: Company.OverdueCount', Company.OverdueCount = 1, IntToStr(Company.OverdueCount));

  Payment := Invoice.AddPayment(692.12, Date);
  Payment.Reference := 'Final';
  ACheck('Paid: Balance', Invoice.Balance = 0, Money(Invoice.Balance));
  ACheck('Paid: IsPaid', Invoice.IsPaid, Bool(Invoice.IsPaid));
  ACheck('Paid: Status', Invoice.Status = 'Paid', Invoice.Status);
  ACheck('Paid: Company.OpenBalance', Company.OpenBalance = 0, Money(Company.OpenBalance));
  ACheck('Paid: Company.OverdueCount', Company.OverdueCount = 0, IntToStr(Company.OverdueCount));
  ACheck('Paid: Person.OverdueCount', Person.OverdueCount = 0, IntToStr(Person.OverdueCount));

  CheckConstraints(Invoice, 'Invoice', 5, ACheck);
  CheckConstraints(Company, 'Company', 3, ACheck);
  CheckConstraints(Settings, 'CompanySettings', 3, ACheck);

  ACheck('Customer.allInstances->size', CountOf(ASystem, 'Customer.allInstances->size') = 2,
    IntToStr(CountOf(ASystem, 'Customer.allInstances->size')));
  ACheck('Company.allInstances->size', CountOf(ASystem, 'Company.allInstances->size') = 1,
    IntToStr(CountOf(ASystem, 'Company.allInstances->size')));
  ACheck('Address.allInstances->size', CountOf(ASystem, 'Address.allInstances->size') = 4,
    IntToStr(CountOf(ASystem, 'Address.allInstances->size')));
end;

procedure VerifyReloadedScenario(ASystem: TBoldSystem; ACheck: TCheckProc);
var
  Invoice: TInvoice;
  Settings: TCompanySettings;
begin
  ACheck('Reloaded: Invoice.allInstances->size', CountOf(ASystem, 'Invoice.allInstances->size') = 1,
    IntToStr(CountOf(ASystem, 'Invoice.allInstances->size')));
  Invoice := ASystem.EvaluateExpressionAsDirectElement('Invoice.allInstances->first') as TInvoice;
  ACheck('Reloaded: Invoice.Number', Invoice.Number = cFirstInvoiceNumber, IntToStr(Invoice.Number));
  ACheck('Reloaded: Invoice.Status', Invoice.Status = 'Paid', Invoice.Status);
  ACheck('Reloaded: Invoice.GrossTotal', Invoice.GrossTotal = 1192.12, Money(Invoice.GrossTotal));
  ACheck('Reloaded: Invoice.Customer.AsString', Invoice.Customer.AsString = 'Acme Oy', Invoice.Customer.AsString);
  ACheck('Reloaded: Invoice.Customer is TCompany', Invoice.Customer is TCompany, Invoice.Customer.ClassName);
  ACheck('Reloaded: Invoice.Customer.Number', Invoice.Customer.Number = 1, IntToStr(Invoice.Customer.Number));
  ACheck('Reloaded: Invoice.BillingAddress.AsText', Invoice.BillingAddress.AsText = cBillingAddressText, Invoice.BillingAddress.AsText);
  ACheck('Reloaded: Company.Contacts.Count', (Invoice.Customer as TCompany).Contacts.Count = 1,
    IntToStr((Invoice.Customer as TCompany).Contacts.Count));
  Settings := TCompanySettings.Instance(ASystem);
  ACheck('Reloaded: Settings.NextInvoiceNumber', Settings.NextInvoiceNumber = cFirstInvoiceNumber + 1, IntToStr(Settings.NextInvoiceNumber));
  ACheck('Reloaded: Settings.NextCustomerNumber', Settings.NextCustomerNumber = 3, IntToStr(Settings.NextCustomerNumber));
  ACheck('Reloaded: Address.allInstances->size', CountOf(ASystem, 'Address.allInstances->size') = 4,
    IntToStr(CountOf(ASystem, 'Address.allInstances->size')));
end;

end.
