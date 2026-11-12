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

  Bold runs on its freestanding environment here, so nothing drains Bold's
  display queue by itself: AppIdle does it from the LCL's idle loop.

  The handles, datasets and grids are created in code: the Lazarus package
  (packages/Lazarus/Bold.lpk) is runtime only, so Bold's components are not on
  the palette yet. }

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
  BoldHandles,
  BoldListHandle,
  BoldDataSet,
  InvoicingSystemFPC;

type
  TfrmInvoicingMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FData: TInvoicingSystem;
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    FCustomerHandle: TBoldListHandle;
    FInvoiceHandle: TBoldListHandle;
    FLineHandle: TBoldListHandle;
    FCustomers: TBoldDataSet;
    FInvoices: TBoldDataSet;
    FLines: TBoldDataSet;
    FCustomerGrid: TDBGrid;
    FInvoiceGrid: TDBGrid;
    FLineGrid: TDBGrid;
    FStatus: TLabel;
    function AddListHandle(ARoot: TBoldElementHandle; const AExpression: string): TBoldListHandle;
    function AddDataSet(AHandle: TBoldListHandle; const AColumns: array of string): TBoldDataSet;
    function AddGrid(const ACaption: string; ADataSet: TDataSet; AAlign: TAlign; AHeight: Integer): TDBGrid;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SaveClick(Sender: TObject);
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
  BoldQueue,
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

{ TfrmInvoicingMain }

procedure TfrmInvoicingMain.FormCreate(Sender: TObject);
var
  Bar: TPanel;
  Save: TButton;
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

  // customers -> their invoices -> the invoice's lines
  FCustomerHandle := AddListHandle(FData.SystemHandle, 'Customer.allInstances');
  FInvoiceHandle := AddListHandle(FCustomerHandle, 'invoices');
  FLineHandle := AddListHandle(FInvoiceHandle, 'lines');
  FCustomers := AddDataSet(FCustomerHandle,
    ['number|Number', 'name|Name', 'email|Email', 'invoicedTotal|Invoiced', 'openBalance|Open balance', 'overdueCount|Overdue']);
  FInvoices := AddDataSet(FInvoiceHandle,
    ['number|Number', 'invoiceDate|Date', 'dueDate|Due', 'status|Status', 'grossTotal|Total', 'balance|Balance']);
  FLines := AddDataSet(FLineHandle,
    ['description|Description', 'quantity|Quantity', 'unitPrice|Unit price', 'vatPercent|VAT %', 'grossAmount|Amount']);

  Bar := TPanel.Create(Self);
  Bar.Parent := Self;
  Bar.Align := alBottom;
  Bar.Height := 36;
  Bar.BevelOuter := bvNone;
  Bar.Caption := '';
  Save := TButton.Create(Self);
  Save.Parent := Bar;
  Save.Align := alRight;
  Save.Width := 120;
  Save.BorderSpacing.Around := 4;
  Save.Caption := 'Save changes';
  Save.OnClick := SaveClick;
  FStatus := TLabel.Create(Self);
  FStatus.Parent := Bar;
  FStatus.Align := alClient;
  FStatus.Layout := tlCenter;
  FStatus.BorderSpacing.Left := 8;

  FCustomerGrid := AddGrid('Customers', FCustomers, alTop, 220);
  FInvoiceGrid := AddGrid('Invoices of the selected customer', FInvoices, alTop, 200);
  FLineGrid := AddGrid('Lines of the selected invoice', FLines, alClient, 0);

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
  FreeAndNil(FData);
  if FSelfTest then
    DeleteFile(FDatabase);
end;

procedure TfrmInvoicingMain.OpenData;
begin
  FData := TInvoicingSystem.Create;
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
  Result.StaticSystemHandle := FData.SystemHandle;
  Result.RootHandle := ARoot;
  Result.Expression := AExpression;
end;

{ AColumns: 'ocl expression|column caption' }
function TfrmInvoicingMain.AddDataSet(AHandle: TBoldListHandle; const AColumns: array of string): TBoldDataSet;
var
  I, Bar: Integer;
  Description: TBoldDataSetFieldDescription;
begin
  Result := TBoldDataSet.Create(Self);
  Result.BoldHandle := AHandle;
  for I := Low(AColumns) to High(AColumns) do
  begin
    Bar := Pos('|', AColumns[I]);
    Description := Result.FieldDescriptions.Add;
    Description.BoldProperties.Expression := Copy(AColumns[I], 1, Bar - 1);
    Description.FieldName := Copy(AColumns[I], Bar + 1, MaxInt);
  end;
  Result.Open;
end;

function TfrmInvoicingMain.AddGrid(const ACaption: string; ADataSet: TDataSet; AAlign: TAlign;
  AHeight: Integer): TDBGrid;
var
  Caption: TLabel;
  Source: TDataSource;
begin
  Caption := TLabel.Create(Self);
  Caption.Parent := Self;
  // Top-aligned controls stack by their position, so each new one goes below
  Caption.Top := ClientHeight;
  Caption.Align := alTop;
  Caption.BorderSpacing.Left := 8;
  Caption.BorderSpacing.Top := 6;
  Caption.BorderSpacing.Bottom := 2;
  Caption.Caption := ACaption;

  Source := TDataSource.Create(Self);
  Source.DataSet := ADataSet;

  Result := TDBGrid.Create(Self);
  Result.Parent := Self;
  if AHeight > 0 then
  begin
    Result.Height := AHeight;
    Result.Top := ClientHeight;
  end;
  Result.Align := AAlign;
  Result.BorderSpacing.Left := 8;
  Result.BorderSpacing.Right := 8;
  Result.BorderSpacing.Bottom := 4;
  Result.ReadOnly := True;
  Result.Options := Result.Options + [dgRowSelect, dgAlwaysShowSelection] - [dgEditing];
  Result.DataSource := Source;
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
var
  Status: string;
begin
  if Assigned(FStatus) and Assigned(FData) and FData.IsOpen then
  begin
    Status := FData.DataFile + '  -  ' +
      IntToStr(FData.BoldSystem.EvaluateExpressionAsInteger('Customer.allInstances->size')) + ' customers, ' +
      IntToStr(FData.BoldSystem.EvaluateExpressionAsInteger('Invoice.allInstances->size')) + ' invoices';
    if FData.BoldSystem.BoldDirty then
      Status := Status + '  -  unsaved changes';
    FStatus.Caption := Status;
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
  // TBoldDataSet refreshes after a Bold-side change through BoldSharedClock,
  // which arrives on the main thread: ProcessMessages delivers it
  Start := GetTickCount64;
  repeat
    TBoldQueueable.DisplayAll;
    Application.ProcessMessages;
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
  Customer: TCustomer;
  Invoice: TInvoice;
  InvoicesShown, LinesShown: Integer;
  NameShown: string;
begin
  Failures := 0;
  Log := TStringList.Create;
  try
    SelfTestTrace('checks start');
    Check('scenario checks while seeding', SeedFailures = 0, IntToStr(SeedFailures) + ' failed');
    Settle;
    Check('customer grid columns', FCustomerGrid.Columns.Count = FCustomers.FieldCount,
      IntToStr(FCustomerGrid.Columns.Count) + ' columns, ' + IntToStr(FCustomers.FieldCount) + ' fields');
    Check('title of the second customer column', FCustomerGrid.Columns[1].Title.Caption = 'Name',
      FCustomerGrid.Columns[1].Title.Caption);
    Check('customers', Rows(FCustomerGrid) > 0, IntToStr(Rows(FCustomerGrid)));
    InvoicesShown := 0;
    LinesShown := 0;
    FCustomers.First;
    while not FCustomers.Eof do
    begin
      Settle;
      Customer := FCustomers.ActiveBoldObject as TCustomer;
      NameShown := FCustomerGrid.Columns[1].Field.AsUnicodeString;
      Check('name of customer ' + IntToStr(Customer.Number) + ' in its row', NameShown = Customer.Name, NameShown);
      Check('invoice rows of customer ' + IntToStr(Customer.Number),
        Rows(FInvoiceGrid) = Customer.Invoices.Count,
        IntToStr(Rows(FInvoiceGrid)) + ' rows, ' + IntToStr(Customer.Invoices.Count) + ' invoices');
      Inc(InvoicesShown, Rows(FInvoiceGrid));
      FInvoices.First;
      while not FInvoices.Eof do
      begin
        Settle;
        Invoice := FInvoices.ActiveBoldObject as TInvoice;
        Check('line rows of invoice ' + IntToStr(Invoice.Number),
          Rows(FLineGrid) = Invoice.Lines.Count,
          IntToStr(Rows(FLineGrid)) + ' rows, ' + IntToStr(Invoice.Lines.Count) + ' lines');
        Inc(LinesShown, Rows(FLineGrid));
        FInvoices.Next;
      end;
      FCustomers.Next;
    end;
    Check('invoices shown over all customers',
      InvoicesShown = FData.BoldSystem.EvaluateExpressionAsInteger('Invoice.allInstances->size'), IntToStr(InvoicesShown));
    Check('some lines shown', LinesShown > 0, IntToStr(LinesShown));
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
