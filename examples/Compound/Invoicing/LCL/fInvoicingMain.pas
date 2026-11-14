{$IFDEF FPC}{$mode delphiunicode}{$ENDIF}
unit fInvoicingMain;

{ The LCL client of the Invoicing example (#117 step 17), the Lazarus twin of
  ../FMX/fInvoicingMain.pas.

  Three grids: the customers, the invoices of the selected customer and the
  lines of the selected invoice. Each grid is an LCL TDBGrid on a TDataSource
  on a TBoldDataSet, and each dataset sits on a TBoldListHandle. The invoice
  handle is rooted on the customer handle with the expression invoices, and
  the line handle on the invoice handle with lines. Selecting a row moves the
  dataset, the dataset moves its handle's current element, and the grids below
  follow with no code. Derived attributes such as openBalance, grossTotal and
  status are ordinary columns.

  Under the invoice grid a TDBLookupComboBox shows the customer of the
  selected invoice and picks another: a single link through datasets. The
  invoice dataset has a field with the expression customer, the dataset of
  the customers to choose from one with the expression self, and those two
  are the combo box's data field and key field. See README.md.

  All of that is in the form file (fInvoicingMain.lfm): the model, the system
  and persistence handles, the list handles, the datasets with their fields,
  the data sources and the grids are components on the form, wired through
  their properties, as the form designer writes them with the design-time
  package dclBold installed (packages/Lazarus). The model is in the form file
  too, in BoldModel1, as it is in a Delphi data module: the handles' OCL
  expressions are checked against it as soon as the form is read. The code
  only names the data file, opens the system and the datasets, and fills an
  empty database with the scenario.

  BoldEnvironmentLCL, named in the uses clause below, makes Bold run on its
  LCL environment: Bold's display queue is drained from the LCL's idle
  handling, with no code in the form. }

interface

uses
  SysUtils,
  Classes,
  Forms,
  Controls,
  Graphics,
  StdCtrls,
  ExtCtrls,
  DB,
  DBGrids,
  DBCtrls,
  BoldHandles,
  BoldListHandle,
  BoldSystemHandle,
  BoldModel,
  BoldPersistenceHandleFileXML,
  BoldDataSet;

type
  TfrmInvoicingMain = class(TForm)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldPersistenceHandleFileXML1: TBoldPersistenceHandleFileXML;
    BoldSystemHandle1: TBoldSystemHandle;
    blhCustomers: TBoldListHandle;
    blhCustomerChoice: TBoldListHandle;
    blhInvoices: TBoldListHandle;
    blhLines: TBoldListHandle;
    bdsCustomers: TBoldDataSet;
    bdsInvoices: TBoldDataSet;
    bdsLines: TBoldDataSet;
    bdsCustomerChoice: TBoldDataSet;
    dsCustomers: TDataSource;
    dsInvoices: TDataSource;
    dsCustomerChoice: TDataSource;
    dsLines: TDataSource;
    lblCustomers: TLabel;
    grdCustomers: TDBGrid;
    lblInvoices: TLabel;
    grdInvoices: TDBGrid;
    pnlInvoice: TPanel;
    lblInvoiceCustomer: TLabel;
    cmbInvoiceCustomer: TDBLookupComboBox;
    lblLines: TLabel;
    grdLines: TDBGrid;
    pnlBar: TPanel;
    btnSave: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure dsInvoicesDataChange(Sender: TObject; Field: TField);
  private
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure OpenData;
    procedure ShowStatus;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
  public
    { Checks what the grids are bound to, as the lpr's --selftest describes,
      and returns the number of failed checks, or 100 after an exception }
    function RunSelfTest: Integer;
  end;

var
  frmInvoicingMain: TfrmInvoicingMain;

{ True when the command line has --selftest, -selftest or /selftest }
function IsSelfTestRun: Boolean;

{ Starts InvoicingLCL.selftest.txt next to the program; the self-test writes
  its steps there as it goes, so a run that stops shows where }
procedure StartSelfTestLog;
procedure SelfTestTrace(const S: string);

implementation

{$R *.lfm}

uses
  BoldEnvironment,
  BoldEnvironmentLCL, // registers Bold's LCL environment
  InvoicingClasses,
  InvoicingScenario;

var
  SeedFailures: Integer = 0;
  SelfTestLog: string = '';

function IsSelfTestRun: Boolean;
var
  I: Integer;
  Param: string;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if SameText(Param, '--selftest') or SameText(Param, '-selftest') or SameText(Param, '/selftest') then
      Result := True;
  end;
end;

procedure WriteLog(const S: string; AAppend: Boolean);
var
  Log: TextFile;
begin
  AssignFile(Log, SelfTestLog);
  if AAppend then
    Append(Log)
  else
    Rewrite(Log);
  try
    Writeln(Log, S);
  finally
    CloseFile(Log);
  end;
end;

procedure StartSelfTestLog;
begin
  SelfTestLog := ChangeFileExt(ParamStr(0), '.selftest.txt');
  WriteLog('InvoicingLCL self-test', False);
end;

procedure SelfTestTrace(const S: string);
begin
  if SelfTestLog <> '' then
    WriteLog(S, True);
end;

{ InvoicingScenario checks what it builds; the client only counts failures }
procedure CountSeedFailure(const AWhat: string; APassed: Boolean; const AActual: string);
begin
  if not APassed then
    Inc(SeedFailures);
end;

function ModelFileName: string;
begin
  // The program is in LCL/bin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
    '..' + PathDelim + '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld');
end;

{ The form file holds a copy of ../Model/Invoicing.bld, which the Delphi
  clients load; the self-test compares the two, so that a change of the model
  is not forgotten here }
function ClassCountOfModelFile: Integer;
var
  Model: TBoldModel;
  ModelText: TStringList;
begin
  Model := TBoldModel.Create(nil);
  ModelText := TStringList.Create;
  try
    ModelText.LoadFromFile(ModelFileName);
    Model.SetFromModelAsString(ModelText);
    Result := Model.MoldModel.Classes.Count;
  finally
    ModelText.Free;
    Model.Free;
  end;
end;

type
  // Reaches the protected Select and Change, which the widgetset calls when
  // the user picks an item
  TComboAccess = class(TDBLookupComboBox);

{ TfrmInvoicingMain }

procedure TfrmInvoicingMain.FormCreate(Sender: TObject);
var
  Guid: TGUID;
begin
  FSelfTest := IsSelfTestRun;
  if FSelfTest then
  begin
    // No window to show an error in: the log gets it, and the exit code is 100
    Application.OnException := SelfTestException;
    CreateGUID(Guid);
    FDatabase := IncludeTrailingPathDelimiter(GetTempDir) + 'InvoicingLCL-selftest-' + GUIDToString(Guid) + '.xml';
  end
  else
    FDatabase := ChangeFileExt(ParamStr(0), '.xml');
  SelfTestTrace('form created; database ' + FDatabase);
  OpenData;
  SelfTestTrace('system open');

  // The handles are chained in the form file; the datasets open on them
  bdsCustomers.Open;
  // The customers to choose from, for the combo box: a dataset of its own,
  // so that the combo box does not move the customer grid
  bdsCustomerChoice.Open;
  bdsInvoices.Open;
  // The link of an invoice to its customer is a field with a key of the
  // customer, which the combo box shows as a name; not a grid column
  bdsInvoices.FieldByName('Customer').Visible := False;
  bdsLines.Open;

  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmInvoicingMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  // The form would free its components in the order of the form file, the
  // model first. Bold's components are freed here instead, each before what
  // it refers to: the datasets (details before masters), the list handles,
  // and only then is the system closed.
  FreeAndNil(bdsLines);
  FreeAndNil(bdsInvoices);
  FreeAndNil(bdsCustomerChoice);
  FreeAndNil(bdsCustomers);
  FreeAndNil(blhLines);
  FreeAndNil(blhInvoices);
  FreeAndNil(blhCustomerChoice);
  FreeAndNil(blhCustomers);
  if BoldSystemHandle1.Active then
  begin
    if BoldSystemHandle1.System.BoldDirty then
      BoldSystemHandle1.System.Discard;
    BoldSystemHandle1.Active := False;
  end;
  FreeAndNil(BoldSystemHandle1);
  FreeAndNil(BoldPersistenceHandleFileXML1);
  FreeAndNil(BoldSystemTypeInfoHandle1);
  FreeAndNil(BoldModel1);
  if FSelfTest then
    DeleteFile(FDatabase);
end;

procedure TfrmInvoicingMain.OpenData;
begin
  // The model came with the form file. A data file that does not exist yet
  // is an empty database.
  BoldPersistenceHandleFileXML1.FileName := FDatabase;
  BoldSystemHandle1.Active := True;
  if BoldSystemHandle1.System.EvaluateExpressionAsInteger('Customer.allInstances->size') = 0 then
  begin
    BuildAndVerifyScenario(BoldSystemHandle1.System, CountSeedFailure);
    BoldSystemHandle1.System.UpdateDatabase;
  end;
end;

procedure TfrmInvoicingMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Done is left alone: Bold's queue has an idle handler of its own
  ShowStatus;
end;

procedure TfrmInvoicingMain.dsInvoicesDataChange(Sender: TObject; Field: TField);
begin
  // Without an invoice there is nothing to pick a customer for: a data-aware
  // control that edits an empty dataset inserts a record, here a new invoice
  cmbInvoiceCustomer.Enabled := bdsInvoices.Active and not bdsInvoices.IsEmpty;
end;

procedure TfrmInvoicingMain.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.System.UpdateDatabase;
  ShowStatus;
end;

procedure TfrmInvoicingMain.ShowStatus;
var
  Status: string;
begin
  if BoldSystemHandle1.Active then
  begin
    Status := FDatabase + '  -  ' +
      IntToStr(BoldSystemHandle1.System.EvaluateExpressionAsInteger('Customer.allInstances->size')) + ' customers, ' +
      IntToStr(BoldSystemHandle1.System.EvaluateExpressionAsInteger('Invoice.allInstances->size')) + ' invoices';
    if BoldSystemHandle1.System.BoldDirty then
      Status := Status + '  -  unsaved changes';
    lblStatus.Caption := Status;
  end;
end;

procedure TfrmInvoicingMain.SelfTestException(Sender: TObject; E: Exception);
begin
  SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
  FSelfTestException := True;
end;

procedure TfrmInvoicingMain.Settle;
var
  Start: QWord;
begin
  // What Application.Run does, for a while and without waiting: messages,
  // then the idle handlers, where the LCL environment drains Bold's display
  // queue. TBoldDataSet refreshes after a Bold-side change through
  // BoldSharedClock, which arrives on the main thread with the messages.
  Start := GetTickCount64;
  repeat
    Application.ProcessMessages;
    Application.Idle(False);
    Sleep(10);
  until GetTickCount64 - Start > 300;
end;

function TfrmInvoicingMain.RunSelfTest: Integer;
var
  Log: TStringList;
  Failures: Integer;

  procedure Check(const AWhat: string; APassed: Boolean; const AActual: string);
  begin
    if APassed then
      Log.Add('  OK    ' + AWhat + ' = ' + AActual)
    else
    begin
      Log.Add('  FAIL  ' + AWhat + ' = ' + AActual);
      Inc(Failures);
    end;
  end;

  { The rows a grid is bound to. The window is not shown in a self-test, and
    a TDBGrid only counts the rows it paints, so the count is its dataset's. }
  function Rows(AGrid: TDBGrid): Integer;
  begin
    Result := AGrid.DataSource.DataSet.RecordCount;
  end;

var
  Customer, OtherCustomer: TCustomer;
  Invoice: TInvoice;
  InvoicesShown, LinesShown, InvoicesBefore, Choice: Integer;
  NameShown: string;
begin
  Failures := 0;
  Log := TStringList.Create;
  try
    SelfTestTrace('checks start');
    Check('the model in the form file has the classes of ' + ExtractFileName(ModelFileName),
      BoldModel1.MoldModel.Classes.Count = ClassCountOfModelFile,
      IntToStr(BoldModel1.MoldModel.Classes.Count) + ' classes in the form, ' + IntToStr(ClassCountOfModelFile) + ' in the file');
    Check('scenario checks while seeding', SeedFailures = 0, IntToStr(SeedFailures) + ' failed');
    Settle;
    Check('customer grid columns', grdCustomers.Columns.Count = bdsCustomers.FieldCount,
      IntToStr(grdCustomers.Columns.Count) + ' columns, ' + IntToStr(bdsCustomers.FieldCount) + ' fields');
    Check('title of the second customer column', grdCustomers.Columns[1].Title.Caption = 'Name',
      grdCustomers.Columns[1].Title.Caption);
    Check('customers', Rows(grdCustomers) > 0, IntToStr(Rows(grdCustomers)));
    InvoicesShown := 0;
    LinesShown := 0;
    bdsCustomers.First;
    while not bdsCustomers.Eof do
    begin
      Settle;
      Customer := bdsCustomers.ActiveBoldObject as TCustomer;
      NameShown := grdCustomers.Columns[1].Field.AsUnicodeString;
      Check('name of customer ' + IntToStr(Customer.Number) + ' in its row', NameShown = Customer.Name, NameShown);
      Check('invoice rows of customer ' + IntToStr(Customer.Number),
        Rows(grdInvoices) = Customer.Invoices.Count,
        IntToStr(Rows(grdInvoices)) + ' rows, ' + IntToStr(Customer.Invoices.Count) + ' invoices');
      Inc(InvoicesShown, Rows(grdInvoices));
      bdsInvoices.First;
      while not bdsInvoices.Eof do
      begin
        Settle;
        Invoice := bdsInvoices.ActiveBoldObject as TInvoice;
        Check('line rows of invoice ' + IntToStr(Invoice.Number),
          Rows(grdLines) = Invoice.Lines.Count,
          IntToStr(Rows(grdLines)) + ' rows, ' + IntToStr(Invoice.Lines.Count) + ' lines');
        Inc(LinesShown, Rows(grdLines));
        bdsInvoices.Next;
      end;
      bdsCustomers.Next;
    end;
    Check('invoices shown over all customers',
      InvoicesShown = BoldSystemHandle1.System.EvaluateExpressionAsInteger('Invoice.allInstances->size'), IntToStr(InvoicesShown));
    Check('some lines shown', LinesShown > 0, IntToStr(LinesShown));

    // Nothing here calls TBoldQueueable.DisplayAll: the LCL environment drains
    // Bold's display queue from the idle handling, which Settle runs
    Check('Bold environment', BoldEffectiveEnvironment.Name = 'LCL', BoldEffectiveEnvironment.Name);
    bdsCustomers.First;
    Check('moving the customer dataset queues followers to display', not BoldInstalledQueue.Empty,
      IntToStr(BoldInstalledQueue.DisplayCount) + ' queued');
    Settle;
    Check('the idle handling drains Bold''s display queue', BoldInstalledQueue.Empty,
      IntToStr(BoldInstalledQueue.DisplayCount) + ' queued');

    // The combo box under the invoice grid: a single link through a dataset.
    // Its list is the dataset of all customers, its key that dataset's field
    // 'self', and its data field the invoice's field 'customer'.
    bdsCustomers.First;
    while not bdsCustomers.Eof and (bdsInvoices.RecordCount = 0) do
      bdsCustomers.Next;
    Settle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    bdsInvoices.First;
    Settle;
    Invoice := bdsInvoices.ActiveBoldObject as TInvoice;
    InvoicesBefore := Customer.Invoices.Count;
    Check('the combo box lists every customer',
      cmbInvoiceCustomer.Items.Count = BoldSystemHandle1.System.EvaluateExpressionAsInteger('Customer.allInstances->size'),
      IntToStr(cmbInvoiceCustomer.Items.Count));
    Check('the combo box shows the customer of the selected invoice',
      (cmbInvoiceCustomer.ItemIndex >= 0) and
      (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = Customer.Name),
      string(cmbInvoiceCustomer.Text));
    // Another customer, picked as a user picks it from the list
    OtherCustomer := nil;
    Choice := 0;
    while (Choice < cmbInvoiceCustomer.Items.Count) and
      (string(cmbInvoiceCustomer.Items[Choice]) = Customer.Name) do
      Inc(Choice);
    cmbInvoiceCustomer.ItemIndex := Choice;
    // What the widgetset calls then: the combo box notes the selection and
    // posts itself a message, on which it edits the dataset
    TComboAccess(cmbInvoiceCustomer).Select;
    TComboAccess(cmbInvoiceCustomer).Change;
    Settle;
    if bdsInvoices.State in dsEditModes then
      bdsInvoices.Post;
    Settle;
    if Assigned(Invoice.Customer) then
      OtherCustomer := Invoice.Customer;
    Check('picking another customer in the combo box links the invoice to it',
      Assigned(OtherCustomer) and (OtherCustomer <> Customer) and
      (OtherCustomer.Name = string(cmbInvoiceCustomer.Items[Choice])),
      'invoice ' + IntToStr(Invoice.Number) + ' of ' + Invoice.Customer.Name);
    Check('and the invoice leaves the invoice grid of its former customer',
      (Customer.Invoices.Count = InvoicesBefore - 1) and (Rows(grdInvoices) = Customer.Invoices.Count),
      IntToStr(Rows(grdInvoices)) + ' rows, ' + IntToStr(Customer.Invoices.Count) + ' invoices');
    Check('the invoice dataset is not left editing', not (bdsInvoices.State in dsEditModes),
      IntToStr(Ord(bdsInvoices.State)));
    if Customer.Invoices.Count = 0 then
      Check('with no invoice left in the grid the combo box shows no customer and cannot be used',
        (cmbInvoiceCustomer.ItemIndex = -1) and not cmbInvoiceCustomer.Enabled,
        string(cmbInvoiceCustomer.Text) + ', enabled ' + BoolToStr(cmbInvoiceCustomer.Enabled, True))
    else
      Check('the combo box shows the customer of the invoice that is now selected',
        (cmbInvoiceCustomer.ItemIndex >= 0) and
        (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = Customer.Name),
        string(cmbInvoiceCustomer.Text));
    // The row of the customer that got the invoice
    if Assigned(OtherCustomer) then
    begin
      bdsCustomers.First;
      while not bdsCustomers.Eof and (bdsCustomers.ActiveBoldObject <> OtherCustomer) do
        bdsCustomers.Next;
      Settle;
      bdsInvoices.First;
      while not bdsInvoices.Eof and (bdsInvoices.ActiveBoldObject <> Invoice) do
        bdsInvoices.Next;
      Settle;
      Check('the invoice is in the invoice grid of its new customer',
        (bdsCustomers.ActiveBoldObject = OtherCustomer) and (bdsInvoices.ActiveBoldObject = Invoice) and
        (Rows(grdInvoices) = OtherCustomer.Invoices.Count),
        IntToStr(Rows(grdInvoices)) + ' rows, ' + IntToStr(OtherCustomer.Invoices.Count) + ' invoices');
      Check('where the combo box shows that customer',
        cmbInvoiceCustomer.Enabled and (cmbInvoiceCustomer.ItemIndex >= 0) and
        (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = OtherCustomer.Name),
        string(cmbInvoiceCustomer.Text));
      Check('and the invoiced total of the customer row has the invoice',
        bdsCustomers.FieldByName('Invoiced').AsCurrency = OtherCustomer.InvoicedTotal,
        bdsCustomers.FieldByName('Invoiced').AsString);
    end;

    // A customer renamed on the Bold side: the grid row and the combo box's list
    bdsCustomers.First;
    Settle;
    // Only an attribute changes, on the Bold side; the change is not saved
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    Customer.Name := Customer.Name + ' (renamed)';
    Settle;
    NameShown := grdCustomers.Columns[1].Field.AsUnicodeString;
    Check('a rename on the Bold side shows in the grid', NameShown = Customer.Name, NameShown);

    Log.Add(IntToStr(Failures) + ' failed');
    SelfTestTrace(Log.Text);
  finally
    Log.Free;
  end;
  if FSelfTestException then
    Result := 100
  else
    Result := Failures;
end;

end.
