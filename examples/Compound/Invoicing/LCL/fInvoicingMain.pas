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
  are the combo box's data field and key field. After a pick the form moves
  the customer grid to the new customer, so that the invoice stays in view.
  See README.md.

  Edit OCL opens Bold's OCL editor as an LCL dialog
  (BoldOclExpressionDialogLCL) on the model, and evaluates the expression it
  gives back. The FMX client (../FMX) has the same form.

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
    btnNewInvoice: TButton;
    btnPostInvoice: TButton;
    pnlLines: TPanel;
    lblLines: TLabel;
    btnAddLine: TButton;
    btnDeleteLine: TButton;
    grdLines: TDBGrid;
    pnlBar: TPanel;
    btnSave: TButton;
    btnEditOcl: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnEditOclClick(Sender: TObject);
    procedure btnNewInvoiceClick(Sender: TObject);
    procedure btnPostInvoiceClick(Sender: TObject);
    procedure btnAddLineClick(Sender: TObject);
    procedure btnDeleteLineClick(Sender: TObject);
    procedure dsInvoicesDataChange(Sender: TObject; Field: TField);
    procedure cmbInvoiceCustomerSelect(Sender: TObject);
  private
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    { What Edit OCL opens with: the expression of the last OK }
    FOclExpression: string;
    // A TInvoice; the model's classes are not named in the interface
    FInvoiceToFollow: TObject;
    procedure FollowTheInvoice(Data: PtrInt);
    procedure UpdateInvoiceControls;
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
  Dialogs,
  BoldQueue,
  BoldOclExpressionCheck,
  BoldOclExpressionDialogLCL,
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
  UpdateInvoiceControls;

  FOclExpression := 'Customer.allInstances';
  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmInvoicingMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  Application.RemoveAsyncCalls(Self);
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
  UpdateInvoiceControls;
  ShowStatus;
end;

procedure TfrmInvoicingMain.dsInvoicesDataChange(Sender: TObject; Field: TField);
begin
  UpdateInvoiceControls;
end;

{ What can be done with the selected invoice. Called when the invoice dataset
  moves or changes, and from the idle loop, where a posting made elsewhere
  arrives too. }
procedure TfrmInvoicingMain.UpdateInvoiceControls;
var
  HasInvoice, IsDraft: Boolean;
begin
  HasInvoice := bdsInvoices.Active and not bdsInvoices.IsEmpty and (bdsInvoices.ActiveBoldObject is TInvoice);
  IsDraft := HasInvoice and not TInvoice(bdsInvoices.ActiveBoldObject).IsPosted;
  // A posted invoice is frozen, which the model says in its business methods
  // (TInvoice.AddLine); the form follows that rule here. So only a draft
  // gets another customer. And without an invoice there is nothing to pick a
  // customer for: a data-aware control that edits an empty dataset inserts a
  // record, here a new invoice.
  cmbInvoiceCustomer.Enabled := IsDraft;
  // The lines of a draft are edited in the grid: Add line (or the down arrow
  // on the last row) adds one, typing in a cell changes it, Delete line (or
  // Ctrl+Del) deletes it.
  grdLines.ReadOnly := not IsDraft;
  btnAddLine.Enabled := IsDraft;
  btnDeleteLine.Enabled := IsDraft and bdsLines.Active and not bdsLines.IsEmpty;
  btnPostInvoice.Enabled := IsDraft;
  btnNewInvoice.Enabled := bdsCustomers.Active and not bdsCustomers.IsEmpty;
end;

procedure TfrmInvoicingMain.btnNewInvoiceClick(Sender: TObject);
var
  Invoice: TInvoice;
begin
  if not (bdsCustomers.ActiveBoldObject is TCustomer) then
    Exit;
  // The business method: dates from the payment term, a copy of the address
  Invoice := TCustomer(bdsCustomers.ActiveBoldObject).NewInvoice;
  // The invoice dataset gets the new invoice when Bold's display queue is
  // drained; at once here, so that the new row can be selected
  TBoldQueueable.DisplayAll;
  bdsInvoices.Last;
  while not bdsInvoices.Bof and (bdsInvoices.ActiveBoldObject <> Invoice) do
    bdsInvoices.Prior;
  // An invoice is made to get lines: the draft has its first one at once,
  // ready to be typed in. The lines dataset follows the selected invoice
  // when the display queue is drained.
  TBoldQueueable.DisplayAll;
  if (bdsInvoices.ActiveBoldObject = Invoice) and bdsLines.IsEmpty then
    btnAddLineClick(Sender)
  else
    UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.btnAddLineClick(Sender: TObject);
begin
  // A new row at the end of the lines grid, ready to be typed in. The
  // dataset asks the list of its handle, the lines of the selected invoice,
  // for the new object, so it is a line of that invoice at once.
  bdsLines.Append;
  if grdLines.CanSetFocus then
  begin
    grdLines.SetFocus;
    grdLines.SelectedIndex := 0;
  end;
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.btnDeleteLineClick(Sender: TObject);
begin
  if not bdsLines.IsEmpty then
    bdsLines.Delete;
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.btnPostInvoiceClick(Sender: TObject);
begin
  if not (bdsInvoices.ActiveBoldObject is TInvoice) then
    Exit;
  // A line that is being typed belongs to the invoice that is posted
  if bdsLines.State in dsEditModes then
    bdsLines.Post;
  // The business method: refuses an invoice without lines, gives the number
  TInvoice(bdsInvoices.ActiveBoldObject).Post;
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.cmbInvoiceCustomerSelect(Sender: TObject);
begin
  // The combo box has written the customer that was picked to the invoice.
  // The invoice grid shows the invoices of the selected customer, so the
  // invoice is about to leave it: the form follows the invoice, once the
  // combo box is done with the pick.
  if bdsInvoices.ActiveBoldObject is TInvoice then
  begin
    FInvoiceToFollow := bdsInvoices.ActiveBoldObject;
    Application.QueueAsyncCall(FollowTheInvoice, 0);
  end;
end;

procedure TfrmInvoicingMain.FollowTheInvoice(Data: PtrInt);
var
  Invoice: TInvoice;
begin
  Invoice := FInvoiceToFollow as TInvoice;
  FInvoiceToFollow := nil;
  if not Assigned(Invoice) or not Assigned(Invoice.Customer) or not bdsCustomers.Active then
    Exit;
  if bdsCustomers.ActiveBoldObject = Invoice.Customer then
    Exit;
  // The datasets follow a change when Bold's display queue is drained, which
  // the idle handling does; here it is drained at once, so that the invoice
  // dataset has let the invoice go before the customer grid moves
  TBoldQueueable.DisplayAll;
  if bdsCustomers.Locate('Number', Invoice.Customer.Number, []) then
  begin
    TBoldQueueable.DisplayAll;
    // By the object, not by its number: a draft has no number yet
    bdsInvoices.First;
    while not bdsInvoices.Eof and (bdsInvoices.ActiveBoldObject <> Invoice) do
      bdsInvoices.Next;
    if bdsInvoices.Eof then
      bdsInvoices.First;
  end;
end;

procedure TfrmInvoicingMain.btnSaveClick(Sender: TObject);
begin
  BoldSystemHandle1.System.UpdateDatabase;
  ShowStatus;
end;

{ The OCL editor checks against the model as the user types; after OK the
  expression is evaluated in the system, and its value or error shown }
procedure TfrmInvoicingMain.btnEditOclClick(Sender: TObject);
var
  Expression, Outcome: string;
begin
  Expression := FOclExpression;
  if not BoldEditOclExpressionLCL(Expression, BoldSystemHandle1.StaticSystemTypeInfo) then
    Exit;
  FOclExpression := Expression;
  try
    Outcome := BoldSystemHandle1.System.EvaluateExpressionAsString(Expression);
  except
    on E: Exception do
      Outcome := string(E.ClassName) + ': ' + E.Message;
  end;
  if Length(Outcome) > 1000 then
    Outcome := Copy(Outcome, 1, 1000) + '...';
  ShowMessage(AnsiString(Expression + LineEnding + LineEnding + Outcome));
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

  { A new invoice, its lines edited as the grid edits them (through the
    dataset of the grid), and posted }
  procedure CheckEditingTheLines;
  var
    Customer: TCustomer;
    Invoice: TInvoice;
    Line: TInvoiceLine;
    InvoicesBefore: Integer;
    Refused: string;
  begin
    bdsCustomers.First;
    Settle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    bdsInvoices.First;
    Settle;
    if bdsInvoices.ActiveBoldObject is TInvoice then
      Check('the lines of a posted invoice cannot be edited in the grid',
        TInvoice(bdsInvoices.ActiveBoldObject).IsPosted and grdLines.ReadOnly and not btnPostInvoice.Enabled,
        'read-only ' + BoolToStr(grdLines.ReadOnly, True));

    InvoicesBefore := Customer.Invoices.Count;
    btnNewInvoice.Click;
    Settle;
    Check('New invoice makes a draft for the selected customer, and selects it',
      (Customer.Invoices.Count = InvoicesBefore + 1) and (bdsInvoices.ActiveBoldObject is TInvoice) and
      not TInvoice(bdsInvoices.ActiveBoldObject).IsPosted and
      (TInvoice(bdsInvoices.ActiveBoldObject).Customer = Customer),
      IntToStr(Customer.Invoices.Count) + ' invoices, status ' + bdsInvoices.FieldByName('Status').AsString);
    if not (bdsInvoices.ActiveBoldObject is TInvoice) then
      Exit;
    Invoice := TInvoice(bdsInvoices.ActiveBoldObject);
    Settle;
    Check('a new draft has its first line, ready to be typed in',
      not grdLines.ReadOnly and btnPostInvoice.Enabled and (bdsLines.State = dsInsert) and
      (bdsLines.RecordCount = 1) and (Invoice.Lines.Count = 1),
      IntToStr(bdsLines.RecordCount) + ' rows, state ' + IntToStr(Ord(bdsLines.State)));
    Check('with a line there, Delete line can be used', btnAddLine.Enabled and btnDeleteLine.Enabled,
      BoolToStr(btnDeleteLine.Enabled, True));
    // The first line is not wanted after all
    btnDeleteLine.Click;
    Settle;
    Check('Delete line takes the new line away again',
      (Invoice.Lines.Count = 0) and (bdsLines.RecordCount = 0) and not (bdsLines.State in dsEditModes),
      IntToStr(Invoice.Lines.Count) + ' lines');
    Check('a draft without lines: Add line can be used, Delete line cannot',
      btnAddLine.Enabled and not btnDeleteLine.Enabled, BoolToStr(btnAddLine.Enabled, True));

    Refused := '';
    try
      btnPostInvoice.Click;
    except
      on E: Exception do
        Refused := E.Message;
    end;
    Check('an invoice without lines is not posted', (Refused <> '') and not Invoice.IsPosted, Refused);

    // The next lines are added by hand
    btnAddLine.Click;
    Settle;
    Check('Add line gives the lines grid a new row to type in',
      (bdsLines.State = dsInsert) and (bdsLines.RecordCount = 1) and (Invoice.Lines.Count = 1),
      IntToStr(bdsLines.RecordCount) + ' rows, state ' + IntToStr(Ord(bdsLines.State)));
    bdsLines.FieldByName('Description').AsString := 'Workshop';
    bdsLines.FieldByName('Quantity').AsFloat := 2;
    bdsLines.FieldByName('Unit price').AsCurrency := 100;
    bdsLines.FieldByName('VAT %').AsFloat := 25;
    bdsLines.Post;
    Settle;
    Check('a line typed in the grid is a line of the invoice',
      (Invoice.Lines.Count = 1) and (bdsLines.RecordCount = 1) and (Invoice.Lines[0].Description = 'Workshop'),
      IntToStr(Invoice.Lines.Count) + ' lines');
    Check('its derived amount is in its row', bdsLines.FieldByName('Amount').AsCurrency = 250,
      bdsLines.FieldByName('Amount').AsString);
    Check('and the total of the invoice row has it', bdsInvoices.FieldByName('Total').AsCurrency = 250,
      bdsInvoices.FieldByName('Total').AsString);
    Check('the derived amount is not a cell to type in', bdsLines.FieldByName('Amount').ReadOnly and
      not bdsLines.FieldByName('Quantity').ReadOnly, BoolToStr(bdsLines.FieldByName('Amount').ReadOnly, True));

    bdsLines.Edit;
    bdsLines.FieldByName('Quantity').AsFloat := 3;
    bdsLines.Post;
    Settle;
    Check('an edited quantity changes the amount and the total',
      (bdsLines.FieldByName('Amount').AsCurrency = 375) and (bdsInvoices.FieldByName('Total').AsCurrency = 375),
      bdsLines.FieldByName('Amount').AsString + ', ' + bdsInvoices.FieldByName('Total').AsString);

    // Esc in a cell
    bdsLines.Edit;
    bdsLines.FieldByName('Quantity').AsFloat := 99;
    bdsLines.Cancel;
    Settle;
    Check('Esc takes back what was typed, and keeps the line',
      (Invoice.Lines.Count = 1) and (Invoice.Lines[0].Quantity = 3) and
      (bdsInvoices.FieldByName('Total').AsCurrency = 375),
      FloatToStr(Invoice.Lines[0].Quantity) + ', total ' + bdsInvoices.FieldByName('Total').AsString);

    // A line is being typed, not posted, and the user selects another
    // invoice: the lines grid shows the lines of that invoice
    bdsLines.Append;
    bdsLines.FieldByName('Description').AsString := 'Being typed';
    bdsLines.FieldByName('Quantity').AsFloat := 2;
    bdsLines.FieldByName('Unit price').AsCurrency := 10;
    Check('the derived amount follows while a new line is typed',
      bdsLines.FieldByName('Amount').AsCurrency = 20, bdsLines.FieldByName('Amount').AsString);
    bdsInvoices.First;
    Settle;
    Check('selecting another invoice while a line is typed shows the lines of that invoice',
      (bdsInvoices.ActiveBoldObject <> Invoice) and not (bdsLines.State in dsEditModes) and
      (bdsLines.RecordCount = (bdsInvoices.ActiveBoldObject as TInvoice).Lines.Count) and
      (bdsLines.ActiveBoldObject is TInvoiceLine) and
      (TInvoiceLine(bdsLines.ActiveBoldObject).Invoice = bdsInvoices.ActiveBoldObject),
      IntToStr(bdsLines.RecordCount) + ' rows, state ' + IntToStr(Ord(bdsLines.State)));
    Check('and the line that was typed stays with its invoice', Invoice.Lines.Count = 2, IntToStr(Invoice.Lines.Count));
    // Back to the draft, where that line is deleted again
    bdsInvoices.Last;
    while not bdsInvoices.Bof and (bdsInvoices.ActiveBoldObject <> Invoice) do
      bdsInvoices.Prior;
    Settle;
    bdsLines.Last;
    Check('back on the draft the typed line is there', (bdsInvoices.ActiveBoldObject = Invoice) and
      (bdsLines.FieldByName('Description').AsString = 'Being typed'), bdsLines.FieldByName('Description').AsString);
    bdsLines.Delete;
    Settle;

    bdsLines.Append;
    bdsLines.FieldByName('Description').AsString := 'Not wanted';
    bdsLines.FieldByName('Quantity').AsFloat := 1;
    bdsLines.Post;
    Settle;
    Check('a second line', (Invoice.Lines.Count = 2) and (bdsLines.RecordCount = 2), IntToStr(Invoice.Lines.Count));
    // Ctrl+Del in the grid
    Line := bdsLines.ActiveBoldObject as TInvoiceLine;
    btnDeleteLine.Click;
    Settle;
    Check('a deleted row is a deleted line',
      (Invoice.Lines.Count = 1) and (bdsLines.RecordCount = 1) and (Invoice.Lines[0] <> Line) and
      (bdsInvoices.FieldByName('Total').AsCurrency = 375),
      IntToStr(Invoice.Lines.Count) + ' lines, total ' + bdsInvoices.FieldByName('Total').AsString);

    btnPostInvoice.Click;
    Settle;
    Check('Post invoice posts it: a number, and the status of an open invoice',
      Invoice.IsPosted and (Invoice.Number > 0) and (bdsInvoices.ActiveBoldObject = Invoice) and
      (bdsInvoices.FieldByName('Status').AsString = Invoice.Status),
      IntToStr(Invoice.Number) + ', ' + bdsInvoices.FieldByName('Status').AsString);
    Check('and its lines can no longer be edited in the grid', grdLines.ReadOnly and not btnPostInvoice.Enabled,
      'read-only ' + BoolToStr(grdLines.ReadOnly, True));
  end;

  { The OCL editor, created as Edit OCL creates it but not shown }
  procedure CheckTheOclEditor;
  var
    Editor: TBoldOclExpressionDialog;
    i, Found: Integer;
  begin
    Editor := TBoldOclExpressionDialog.CreateDialog(nil, BoldSystemHandle1.StaticSystemTypeInfo, nil);
    try
      Found := -1;
      for i := 0 to Editor.CompletionCount - 1 do
        if Editor.Completions[i].InsertText = 'Customer' then
          Found := i;
      Check('OCL editor: the classes of the model for the empty expression',
        (Found >= 0) and (Editor.Completions[Found].Kind = ockType), IntToStr(Editor.CompletionCount) + ' lines');
      Editor.Expression := FOclExpression;
      Check('OCL editor: the expression Edit OCL opens with is valid, with its type',
        Editor.LastCheck.Valid and (Editor.LastCheck.ResultTypeName = 'Collection(Customer)'),
        Editor.LastCheck.ResultTypeName);
      Editor.Expression := 'Customer.allInstances.nosuch';
      Check('OCL editor: an error is shown with a pointer at it',
        not Editor.LastCheck.Valid and (Pos('^', Editor.StatusText) > 0), IntToStr(Editor.LastCheck.ErrorPosition));
    finally
      Editor.Free;
    end;
  end;

  { Picks an item of the combo box as a user does. The widgetset then calls
    Select and Change: the combo box notes the selection and posts itself a
    message, on which it edits the dataset. }
  procedure PickInTheComboBox(AIndex: Integer);
  begin
    cmbInvoiceCustomer.ItemIndex := AIndex;
    TComboAccess(cmbInvoiceCustomer).Select;
    TComboAccess(cmbInvoiceCustomer).Change;
    Settle;
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
    while not bdsCustomers.Eof and (bdsInvoices.RecordCount > 0) do
      bdsCustomers.Next;
    Settle;
    if not bdsCustomers.Eof then
      Check('a customer without invoices: an empty invoice grid, and the combo box cannot be used',
        (Rows(grdInvoices) = 0) and (cmbInvoiceCustomer.ItemIndex = -1) and not cmbInvoiceCustomer.Enabled,
        IntToStr(Rows(grdInvoices)) + ' rows, enabled ' + BoolToStr(cmbInvoiceCustomer.Enabled, True));
    bdsCustomers.First;
    while not bdsCustomers.Eof and (bdsInvoices.RecordCount = 0) do
      bdsCustomers.Next;
    Settle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    bdsInvoices.First;
    Settle;
    Check('the combo box lists every customer',
      cmbInvoiceCustomer.Items.Count = BoldSystemHandle1.System.EvaluateExpressionAsInteger('Customer.allInstances->size'),
      IntToStr(cmbInvoiceCustomer.Items.Count));
    Check('the combo box shows the customer of the selected invoice',
      (cmbInvoiceCustomer.ItemIndex >= 0) and
      (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = Customer.Name),
      string(cmbInvoiceCustomer.Text));
    Check('a posted invoice is frozen: it cannot get another customer',
      (bdsInvoices.ActiveBoldObject as TInvoice).IsPosted and not cmbInvoiceCustomer.Enabled,
      'enabled ' + BoolToStr(cmbInvoiceCustomer.Enabled, True));

    // A draft gets another customer, picked as a user picks it from the list
    btnNewInvoice.Click;
    Settle;
    Invoice := bdsInvoices.ActiveBoldObject as TInvoice;
    InvoicesBefore := Customer.Invoices.Count;
    Check('for a draft the combo box can be used, and shows its customer',
      not Invoice.IsPosted and cmbInvoiceCustomer.Enabled and (cmbInvoiceCustomer.ItemIndex >= 0) and
      (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = Customer.Name),
      string(cmbInvoiceCustomer.Text));
    OtherCustomer := nil;
    Choice := 0;
    while (Choice < cmbInvoiceCustomer.Items.Count) and
      (string(cmbInvoiceCustomer.Items[Choice]) = Customer.Name) do
      Inc(Choice);
    PickInTheComboBox(Choice);
    if Assigned(Invoice.Customer) then
      OtherCustomer := Invoice.Customer;
    Check('picking another customer in the combo box links the invoice to it',
      Assigned(OtherCustomer) and (OtherCustomer <> Customer) and
      (OtherCustomer.Name = string(cmbInvoiceCustomer.Items[Choice])),
      'the draft of ' + Invoice.Customer.Name);
    Check('the invoice is no longer among the invoices of its former customer',
      Customer.Invoices.Count = InvoicesBefore - 1, IntToStr(Customer.Invoices.Count) + ' invoices');
    Check('the invoice dataset is not left editing', not (bdsInvoices.State in dsEditModes),
      IntToStr(Ord(bdsInvoices.State)));
    // The form follows the invoice, so that it stays in view
    Check('the customer grid has moved to the new customer', bdsCustomers.ActiveBoldObject = OtherCustomer,
      bdsCustomers.FieldByName('Name').AsString);
    Check('the invoice grid shows that customer''s invoices, with the invoice selected',
      (bdsInvoices.ActiveBoldObject = Invoice) and Assigned(OtherCustomer) and
      (Rows(grdInvoices) = OtherCustomer.Invoices.Count),
      IntToStr(Rows(grdInvoices)) + ' rows');
    Check('the combo box shows the new customer and can be used again',
      cmbInvoiceCustomer.Enabled and (cmbInvoiceCustomer.ItemIndex = Choice),
      string(cmbInvoiceCustomer.Text));

    // And back again, from where the form went
    PickInTheComboBox(cmbInvoiceCustomer.Items.IndexOf(AnsiString(Customer.Name)));
    Check('picking the former customer brings the invoice back to it',
      (Invoice.Customer = Customer) and (Customer.Invoices.Count = InvoicesBefore),
      'the draft of ' + Invoice.Customer.Name);
    Check('and the form follows it there',
      (bdsCustomers.ActiveBoldObject = Customer) and (bdsInvoices.ActiveBoldObject = Invoice) and
      cmbInvoiceCustomer.Enabled and
      (string(cmbInvoiceCustomer.Items[cmbInvoiceCustomer.ItemIndex]) = Customer.Name),
      bdsCustomers.FieldByName('Name').AsString + ', ' + string(cmbInvoiceCustomer.Text));

    // A customer renamed on the Bold side: the grid row and the combo box's list
    bdsCustomers.First;
    Settle;
    // Only an attribute changes, on the Bold side; the change is not saved
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    Customer.Name := Customer.Name + ' (renamed)';
    Settle;
    NameShown := grdCustomers.Columns[1].Field.AsUnicodeString;
    Check('a rename on the Bold side shows in the grid', NameShown = Customer.Name, NameShown);

    CheckTheOclEditor;
    CheckEditingTheLines;
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
