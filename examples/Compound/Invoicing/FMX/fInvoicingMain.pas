unit fInvoicingMain;

{ The FMX client of the Invoicing example (#117 step 10).

  Three grids: the customers, the invoices of the selected customer and the
  lines of the selected invoice. Each grid shows a TBoldDataSet through
  LiveBindings (a TBindSourceDB and a TLinkGridToDataSource), and each dataset
  sits on a TBoldListHandle. The invoice handle is rooted on the customer
  handle with the expression invoices, and the line handle on the invoice
  handle with lines. Selecting a row moves the dataset, the dataset moves its
  handle's current element, and the grids below follow with no code. Derived
  attributes such as openBalance, grossTotal and status are ordinary columns.

  The project defines BOLD_NO_VCL, so Bold's control packs link no VCL unit.
  Without the VCL environment nothing drains Bold's display queue by itself:
  AppIdle does it from FMX's idle loop.

  The handles, datasets, grids and bindings are created in code, because
  TBoldDataSet is not on the component palette yet (#117 step 8). }

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Rtti,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Grid,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  Data.DB,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.Grid,
  Data.Bind.EngExt,
  Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid,
  Fmx.Bind.Editors,
  System.Bindings.Outputs,
  BoldHandles,
  BoldListHandle,
  BoldDataSet,
  dmInvoicing;

type
  TfrmInvoicingMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FData: TdmInvoicing;
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    FCustomerHandle: TBoldListHandle;
    FInvoiceHandle: TBoldListHandle;
    FLineHandle: TBoldListHandle;
    FCustomers: TBoldDataSet;
    FInvoices: TBoldDataSet;
    FLines: TBoldDataSet;
    FCustomerGrid: TStringGrid;
    FInvoiceGrid: TStringGrid;
    FLineGrid: TStringGrid;
    FStatus: TLabel;
    function AddListHandle(ARoot: TBoldElementHandle; const AExpression: string): TBoldListHandle;
    function AddDataSet(AHandle: TBoldListHandle; const AColumns: array of string): TBoldDataSet;
    function AddGrid(const ACaption: string; ADataSet: TDataSet; AAlign: TAlignLayout; AHeight: Single): TStringGrid;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SaveClick(Sender: TObject);
    procedure OpenData;
    procedure ShowStatus;
    procedure Settle;
    procedure SelfTestException(Sender: TObject; E: Exception);
  public
    { Checks what the grids show, as the dpr's --selftest describes, and
      returns the number of failed checks, or 100 after an exception }
    function RunSelfTest: Integer;
  end;

var
  frmInvoicingMain: TfrmInvoicingMain;

{ True when the command line has --selftest, -selftest or /selftest }
function IsSelfTestRun: Boolean;

{ Starts InvoicingFMX.selftest.txt next to the exe; the self-test writes its
  steps there as it goes, so a run that stops shows where }
procedure StartSelfTestLog;
procedure SelfTestTrace(const S: string);

implementation

{$R *.fmx}

uses
  System.IOUtils,
  System.StrUtils,
  BoldQueue,
  InvoicingClasses,
  InvoicingScenario;

var
  SeedFailures: Integer = 0;
  SelfTestLog: string = '';

function IsSelfTestRun: Boolean;
begin
  // FindCmdLineSwitch strips one switch character, so --selftest needs its own check
  Result := FindCmdLineSwitch('selftest', ['-', '/'], True) or FindCmdLineSwitch('-selftest', ['-'], True);
end;

procedure StartSelfTestLog;
begin
  SelfTestLog := ChangeFileExt(ParamStr(0), '.selftest.txt');
  TFile.WriteAllText(SelfTestLog, 'InvoicingFMX self-test' + sLineBreak);
end;

procedure SelfTestTrace(const S: string);
begin
  if SelfTestLog <> '' then
    TFile.AppendAllText(SelfTestLog, S + sLineBreak);
end;

{ InvoicingScenario checks what it builds; the client only counts failures }
procedure CountSeedFailure(const AWhat: string; APassed: Boolean; const AActual: string);
begin
  if not APassed then
    Inc(SeedFailures);
end;

function ModelFileName: string;
begin
  Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
    '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld'));
end;

{ TfrmInvoicingMain }

procedure TfrmInvoicingMain.FormCreate(Sender: TObject);
var
  Bar: TLayout;
  Save: TButton;
begin
  FSelfTest := IsSelfTestRun;
  if FSelfTest then
  begin
    // No window to show an error in: the log gets it, and the exit code is 100
    Application.OnException := SelfTestException;
    FDatabase := TPath.Combine(TPath.GetTempPath, 'InvoicingFMX-selftest-' + TGUID.NewGuid.ToString + '.db');
  end
  else
    FDatabase := ChangeFileExt(ParamStr(0), '.db');
  SelfTestTrace('form created; database ' + FDatabase);
  OpenData;
  SelfTestTrace('system open');

  // customers -> their invoices -> the invoice's lines
  FCustomerHandle := AddListHandle(FData.BoldSystemHandle1, 'Customer.allInstances');
  FInvoiceHandle := AddListHandle(FCustomerHandle, 'invoices');
  FLineHandle := AddListHandle(FInvoiceHandle, 'lines');
  FCustomers := AddDataSet(FCustomerHandle,
    ['number|Number', 'name|Name', 'email|Email', 'invoicedTotal|Invoiced', 'openBalance|Open balance', 'overdueCount|Overdue']);
  FInvoices := AddDataSet(FInvoiceHandle,
    ['number|Number', 'invoiceDate|Date', 'dueDate|Due', 'status|Status', 'grossTotal|Total', 'balance|Balance']);
  FLines := AddDataSet(FLineHandle,
    ['description|Description', 'quantity|Quantity', 'unitPrice|Unit price', 'vatPercent|VAT %', 'grossAmount|Amount']);

  Bar := TLayout.Create(Self);
  Bar.Parent := Self;
  Bar.Align := TAlignLayout.Bottom;
  Bar.Height := 36;
  FStatus := TLabel.Create(Self);
  FStatus.Parent := Bar;
  FStatus.Align := TAlignLayout.Client;
  FStatus.Margins.Left := 8;
  Save := TButton.Create(Self);
  Save.Parent := Bar;
  Save.Align := TAlignLayout.Right;
  Save.Width := 120;
  Save.Margins.Rect := TRectF.Create(4, 4, 8, 4);
  Save.Text := 'Save changes';
  Save.OnClick := SaveClick;

  FCustomerGrid := AddGrid('Customers', FCustomers, TAlignLayout.Top, 220);
  FInvoiceGrid := AddGrid('Invoices of the selected customer', FInvoices, TAlignLayout.Top, 200);
  FLineGrid := AddGrid('Lines of the selected invoice', FLines, TAlignLayout.Client, 0);

  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmInvoicingMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  // Details before masters, datasets before their handles, all before the system
  FreeAndNil(FLines);
  FreeAndNil(FInvoices);
  FreeAndNil(FCustomers);
  FreeAndNil(FLineHandle);
  FreeAndNil(FInvoiceHandle);
  FreeAndNil(FCustomerHandle);
  if Assigned(FData) and FData.IsOpen then
    FData.CloseSystem(False);
  if FSelfTest then
    System.SysUtils.DeleteFile(FDatabase);
end;

procedure TfrmInvoicingMain.OpenData;
begin
  FData := TdmInvoicing.Create(Self);
  FData.LoadModelFromFile(ModelFileName);
  SelfTestTrace('model loaded from ' + ModelFileName);
  FData.OpenSystem(FDatabase);
  if FData.BoldSystem.EvaluateExpressionAsInteger('Customer.allInstances->size') = 0 then
  begin
    BuildAndVerifyScenario(FData.BoldSystem, CountSeedFailure);
    FData.Save;
  end;
end;

function TfrmInvoicingMain.AddListHandle(ARoot: TBoldElementHandle; const AExpression: string): TBoldListHandle;
begin
  Result := TBoldListHandle.Create(Self);
  Result.StaticSystemHandle := FData.BoldSystemHandle1;
  Result.RootHandle := ARoot;
  Result.Expression := AExpression;
end;

{ AColumns: 'ocl expression|column caption' }
function TfrmInvoicingMain.AddDataSet(AHandle: TBoldListHandle; const AColumns: array of string): TBoldDataSet;
var
  Column: string;
  Parts: TArray<string>;
  Description: TBoldDataSetFieldDescription;
begin
  Result := TBoldDataSet.Create(Self);
  Result.BoldHandle := AHandle;
  for Column in AColumns do
  begin
    Parts := Column.Split(['|']);
    Description := Result.FieldDescriptions.Add;
    Description.BoldProperties.Expression := Parts[0];
    Description.FieldName := Parts[1];
  end;
  Result.Open;
end;

function TfrmInvoicingMain.AddGrid(const ACaption: string; ADataSet: TDataSet; AAlign: TAlignLayout;
  AHeight: Single): TStringGrid;
var
  Caption: TLabel;
  Source: TBindSourceDB;
  Link: TLinkGridToDataSource;
begin
  Caption := TLabel.Create(Self);
  Caption.Parent := Self;
  Caption.Align := TAlignLayout.Top;
  Caption.Margins.Rect := TRectF.Create(8, 6, 8, 2);
  Caption.Text := ACaption;
  if AAlign = TAlignLayout.Top then
    // Top-aligned controls stack in creation order only by their position
    Caption.Position.Y := ClientHeight;

  Result := TStringGrid.Create(Self);
  Result.Parent := Self;
  Result.Align := AAlign;
  Result.Margins.Rect := TRectF.Create(8, 0, 8, 4);
  if AHeight > 0 then
  begin
    Result.Height := AHeight;
    Result.Position.Y := ClientHeight;
  end;
  Result.Options := Result.Options + [TGridOption.RowSelect, TGridOption.AlwaysShowSelection] - [TGridOption.Editing];

  Source := TBindSourceDB.Create(Self);
  Source.DataSet := ADataSet;
  Link := TLinkGridToDataSource.Create(Self);
  Link.DataSource := Source;
  Link.GridControl := Result;
  Link.Active := True;
end;

procedure TfrmInvoicingMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Brings the Bold-aware followers, TBoldDataSet's among them, up to date
  TBoldQueueable.DisplayAll;
  ShowStatus;
  Done := True;
end;

procedure TfrmInvoicingMain.SaveClick(Sender: TObject);
begin
  FData.Save;
  ShowStatus;
end;

procedure TfrmInvoicingMain.ShowStatus;
begin
  if Assigned(FStatus) and Assigned(FData) and FData.IsOpen then
    FStatus.Text := Format('%s  -  %d customers, %d invoices%s', [FData.Database,
      FData.BoldSystem.EvaluateExpressionAsInteger('Customer.allInstances->size'),
      FData.BoldSystem.EvaluateExpressionAsInteger('Invoice.allInstances->size'),
      IfThen(FData.BoldSystem.BoldDirty, '  -  unsaved changes', '')]);
end;

procedure TfrmInvoicingMain.SelfTestException(Sender: TObject; E: Exception);
begin
  // No Halt: the dpr frees the form before Bold's units are finalized
  SelfTestTrace('EXCEPTION ' + E.ClassName + ': ' + E.Message);
  FSelfTestException := True;
end;

procedure TfrmInvoicingMain.Settle;
var
  Start: Cardinal;
begin
  Start := TThread.GetTickCount;
  repeat
    TBoldQueueable.DisplayAll;
    CheckSynchronize(10);
    Application.ProcessMessages;
  until TThread.GetTickCount - Start > 300;
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

var
  Customer: TCustomer;
  Invoice: TInvoice;
  InvoicesShown, LinesShown: Integer;
begin
  Failures := 0;
  Log := TStringList.Create;
  try
    SelfTestTrace('checks start');
    Check('scenario checks while seeding', SeedFailures = 0, IntToStr(SeedFailures) + ' failed');
    Settle;
    Check('customer rows', FCustomerGrid.RowCount = FCustomers.RecordCount,
      Format('%d rows, %d customers', [FCustomerGrid.RowCount, FCustomers.RecordCount]));
    Check('customers', FCustomers.RecordCount > 0, IntToStr(FCustomers.RecordCount));
    InvoicesShown := 0;
    LinesShown := 0;
    FCustomers.First;
    while not FCustomers.Eof do
    begin
      Settle;
      Customer := FCustomers.ActiveBoldObject as TCustomer;
      Check('name of customer ' + IntToStr(Customer.Number) + ' in its row',
        FCustomerGrid.Cells[1, FCustomers.RecNo - 1] = Customer.Name, FCustomerGrid.Cells[1, FCustomers.RecNo - 1]);
      Check('invoice rows of customer ' + IntToStr(Customer.Number),
        FInvoiceGrid.RowCount = Customer.Invoices.Count,
        Format('%d rows, %d invoices', [FInvoiceGrid.RowCount, Customer.Invoices.Count]));
      Inc(InvoicesShown, FInvoiceGrid.RowCount);
      FInvoices.First;
      while not FInvoices.Eof do
      begin
        Settle;
        Invoice := FInvoices.ActiveBoldObject as TInvoice;
        Check('line rows of invoice ' + IntToStr(Invoice.Number),
          FLineGrid.RowCount = Invoice.Lines.Count,
          Format('%d rows, %d lines', [FLineGrid.RowCount, Invoice.Lines.Count]));
        Inc(LinesShown, FLineGrid.RowCount);
        FInvoices.Next;
      end;
      FCustomers.Next;
    end;
    Check('invoices shown over all customers',
      InvoicesShown = FData.BoldSystem.EvaluateExpressionAsInteger('Invoice.allInstances->size'), IntToStr(InvoicesShown));
    Check('some lines shown', LinesShown > 0, IntToStr(LinesShown));
    Log.Add(Format('%d failed', [Failures]));
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
