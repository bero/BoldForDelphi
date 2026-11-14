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
  TBoldDataSet is not on the component palette yet (#117 step 8).

  Under the invoice grid a combo box shows the customer of the selected
  invoice and picks another: a single link through datasets. The invoice
  dataset has a field with the expression customer, the dataset of the
  customers to choose from one with the expression self, and a
  TLinkFillControlToField binds the combo box to the two. After a pick the
  form moves the customer grid to the new customer, so that the invoice
  stays in view. The LCL client (../LCL) shows the same.

  Edit OCL opens Bold's OCL editor as an FMX dialog (BoldOclExpressionDialogFMX,
  #124) on the Invoicing model, and evaluates the expression it gives back. }

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
  FMX.ListBox,
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
    FCustomerChoiceHandle: TBoldListHandle;
    FCustomerChoice: TBoldDataSet;
    FInvoiceCustomer: TComboBox;
    FInvoiceSource: TBindSourceDB;
    FFollowQueued: Boolean;
    // A TInvoice; the model's classes are not named in the interface
    FInvoiceToFollow: TObject;
    FCustomers: TBoldDataSet;
    FInvoices: TBoldDataSet;
    FLines: TBoldDataSet;
    FCustomerGrid: TStringGrid;
    FInvoiceGrid: TStringGrid;
    FLineGrid: TStringGrid;
    FStatus: TLabel;
    { What Edit OCL opens with: the expression of the last OK }
    FOclExpression: string;
    function AddListHandle(ARoot: TBoldElementHandle; const AExpression: string): TBoldListHandle;
    function AddDataSet(AHandle: TBoldListHandle; const AColumns: array of string): TBoldDataSet;
    function AddGrid(const ACaption: string; ADataSet: TDataSet; AAlign: TAlignLayout; AHeight: Single;
      out ASource: TBindSourceDB): TStringGrid;
    procedure AddInvoiceCustomerRow;
    procedure InvoiceCustomerChange(Sender: TObject);
    procedure FollowTheInvoice;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure SaveClick(Sender: TObject);
    procedure EditOclClick(Sender: TObject);
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
  {$IFDEF POSIX}
  Posix.Unistd, // SysUtils.DeleteFile is inlined from it (H2443 without)
  {$ENDIF}
  FMX.DialogService,
  BoldQueue,
  BoldOclExpressionCheck,
  BoldOclExpressionDialogFMX,
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

{ A deployed program (Linux64 through PAServer) has the model next to it; one
  run where it was built finds it in the repository's Model folder }
function ModelFileName: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Invoicing.bld');
  if not TFile.Exists(Result) then
    Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)),
      '..' + PathDelim + 'Model' + PathDelim + 'Invoicing.bld'));
end;

{ TfrmInvoicingMain }

procedure TfrmInvoicingMain.FormCreate(Sender: TObject);
var
  Bar: TLayout;
  Save, EditOcl: TButton;
  Source: TBindSourceDB;
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
    ['number|Number', 'invoiceDate|Date', 'dueDate|Due', 'status|Status', 'grossTotal|Total', 'balance|Balance',
     'customer|Customer']);
  // The link to the customer is a key, which the combo box shows as a name;
  // not a grid column
  FInvoices.FieldByName('Customer').Visible := False;
  // The customers to choose from, for the combo box: a dataset of its own,
  // so that the combo box does not move the customer grid. Its field self is
  // the key that the invoices' field customer holds.
  FCustomerChoiceHandle := AddListHandle(FData.BoldSystemHandle1, 'Customer.allInstances');
  FCustomerChoice := AddDataSet(FCustomerChoiceHandle, ['self|self', 'name|Name']);
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
  // FMX lines up right-aligned controls by their right edge, and Save has
  // moved to the far right: at position 0 this button goes left of it
  EditOcl := TButton.Create(Self);
  EditOcl.Parent := Bar;
  EditOcl.Position.X := 0;
  EditOcl.Align := TAlignLayout.Right;
  EditOcl.Width := 120;
  EditOcl.Margins.Rect := TRectF.Create(4, 4, 4, 4);
  EditOcl.Text := 'Edit OCL...';
  EditOcl.OnClick := EditOclClick;
  FOclExpression := 'Customer.allInstances';

  FCustomerGrid := AddGrid('Customers', FCustomers, TAlignLayout.Top, 220, Source);
  FInvoiceGrid := AddGrid('Invoices of the selected customer', FInvoices, TAlignLayout.Top, 164, FInvoiceSource);
  AddInvoiceCustomerRow;
  FLineGrid := AddGrid('Lines of the selected invoice', FLines, TAlignLayout.Client, 0, Source);

  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmInvoicingMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  FInvoiceToFollow := nil;
  // Details before masters, datasets before their handles, all before the system
  FreeAndNil(FLines);
  FreeAndNil(FInvoices);
  FreeAndNil(FCustomerChoice);
  FreeAndNil(FCustomers);
  FreeAndNil(FLineHandle);
  FreeAndNil(FInvoiceHandle);
  FreeAndNil(FCustomerChoiceHandle);
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
  AHeight: Single; out ASource: TBindSourceDB): TStringGrid;
var
  Caption: TLabel;
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

  ASource := TBindSourceDB.Create(Self);
  ASource.DataSet := ADataSet;
  Link := TLinkGridToDataSource.Create(Self);
  Link.DataSource := ASource;
  Link.GridControl := Result;
  Link.Active := True;
end;

{ The row under the invoice grid: the customer of the selected invoice in a
  combo box. LiveBindings' lookup link fills the combo box from the choice
  dataset (the names to show, and with each the key in the field self), and
  binds its selection to the invoices' field customer, which holds such a
  key: it shows the customer of the selected invoice, and a pick writes the
  key of the customer that was picked. }
procedure TfrmInvoicingMain.AddInvoiceCustomerRow;
var
  Row: TLayout;
  Caption: TLabel;
  ChoiceSource: TBindSourceDB;
  Link: TLinkFillControlToField;
begin
  Row := TLayout.Create(Self);
  Row.Parent := Self;
  Row.Align := TAlignLayout.Top;
  Row.Height := 36;
  Row.Position.Y := ClientHeight;
  Caption := TLabel.Create(Self);
  Caption.Parent := Row;
  Caption.Align := TAlignLayout.Left;
  Caption.Width := 250;
  Caption.Margins.Left := 8;
  Caption.Text := 'Customer of the selected invoice:';
  FInvoiceCustomer := TComboBox.Create(Self);
  FInvoiceCustomer.Parent := Row;
  FInvoiceCustomer.Align := TAlignLayout.Left;
  FInvoiceCustomer.Position.X := 300;
  FInvoiceCustomer.Width := 320;
  FInvoiceCustomer.Margins.Rect := TRectF.Create(4, 4, 4, 4);

  ChoiceSource := TBindSourceDB.Create(Self);
  ChoiceSource.DataSet := FCustomerChoice;
  Link := TLinkFillControlToField.Create(Self);
  Link.DataSource := FInvoiceSource;
  Link.FieldName := 'Customer';
  Link.Control := FInvoiceCustomer;
  Link.FillDataSource := ChoiceSource;
  Link.FillDisplayFieldName := 'Name';
  Link.FillValueFieldName := 'self';
  Link.AutoFill := True;
  Link.Active := True;
  // After the link, which has its own way of hearing a pick
  FInvoiceCustomer.OnChange := InvoiceCustomerChange;
end;

procedure TfrmInvoicingMain.InvoiceCustomerChange(Sender: TObject);
begin
  // A pick, or the invoice grid moved to another invoice. After a pick the
  // invoice is about to leave the invoice grid, which shows the invoices of
  // the selected customer: the form follows the invoice, once the combo box
  // and its link are done with the pick.
  // The invoice is noted now: once the pick is written it is no longer the
  // selected one.
  if Assigned(FInvoices) and FInvoices.Active and not FInvoices.IsEmpty then
    FInvoiceToFollow := FInvoices.ActiveBoldObject;
  if not FFollowQueued then
  begin
    FFollowQueued := True;
    TThread.ForceQueue(nil, FollowTheInvoice);
  end;
end;

procedure TfrmInvoicingMain.FollowTheInvoice;
var
  Invoice: TInvoice;
begin
  FFollowQueued := False;
  if not Assigned(FInvoices) or not Assigned(FCustomers) or not (FInvoiceToFollow is TInvoice) then
    Exit;
  Invoice := TInvoice(FInvoiceToFollow);
  FInvoiceToFollow := nil;
  if not Assigned(Invoice.Customer) or (FCustomers.ActiveBoldObject = Invoice.Customer) then
    Exit;
  // The datasets follow a change when Bold's display queue is drained, which
  // AppIdle does; here it is drained at once, so that the invoice dataset
  // has let the invoice go before the customer grid moves
  TBoldQueueable.DisplayAll;
  if FCustomers.Locate('Number', Invoice.Customer.Number, []) then
  begin
    TBoldQueueable.DisplayAll;
    FInvoices.Locate('Number', Invoice.Number, []);
  end;
end;

procedure TfrmInvoicingMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Brings the Bold-aware followers, TBoldDataSet's among them, up to date
  TBoldQueueable.DisplayAll;
  // Without an invoice there is nothing to pick a customer for
  if Assigned(FInvoiceCustomer) and Assigned(FInvoices) then
    FInvoiceCustomer.Enabled := FInvoices.Active and not FInvoices.IsEmpty;
  ShowStatus;
  Done := True;
end;

procedure TfrmInvoicingMain.SaveClick(Sender: TObject);
begin
  FData.Save;
  ShowStatus;
end;

{ The OCL editor checks against the model as the user types; after OK the
  expression is evaluated in the system, and its value or error shown }
procedure TfrmInvoicingMain.EditOclClick(Sender: TObject);
var
  Expression, Outcome: string;
begin
  Expression := FOclExpression;
  if not BoldEditOclExpressionFMX(Expression, FData.BoldSystemHandle1.StaticSystemTypeInfo) then
    Exit;
  FOclExpression := Expression;
  try
    Outcome := FData.BoldSystem.EvaluateExpressionAsString(Expression);
  except
    on E: Exception do
      Outcome := E.ClassName + ': ' + E.Message;
  end;
  if Length(Outcome) > 1000 then
    Outcome := Copy(Outcome, 1, 1000) + '...';
  TDialogService.ShowMessage(Expression + sLineBreak + sLineBreak + Outcome);
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

  function OneLine(const S: string): string;
  begin
    Result := StringReplace(StringReplace(S, #13, '', [rfReplaceAll]), #10, ' | ', [rfReplaceAll]);
  end;

  function IndexOfCompletion(AEditor: TBoldOclExpressionDialogFMX; const AInsertText: string): Integer;
  begin
    for Result := 0 to AEditor.CompletionCount - 1 do
      if AEditor.Completions[Result].InsertText = AInsertText then
        Exit;
    Result := -1;
  end;

  { The FMX OCL editor, created as Edit OCL creates it but not shown: the
    steps of a user, through the dialog's methods and its memo }
  procedure CheckTheOclEditor;
  const
    cSelect = 'Customer.allInstances->select(«Boolean»)';
  var
    Editor: TBoldOclExpressionDialogFMX;
    Index: Integer;
  begin
    Editor := TBoldOclExpressionDialogFMX.CreateDialog(nil, FData.BoldSystemHandle1.StaticSystemTypeInfo, nil);
    try
      Check('OCL editor: the icons of its lists', Editor.IconCount = 5, IntToStr(Editor.IconCount) + ' icons');
      Check('OCL editor: the classes for the empty expression', (Editor.StatusText = 'Empty Expression') and
        (IndexOfCompletion(Editor, 'Customer') >= 0) and (Editor.Completions[0].Kind = ockType),
        IntToStr(Editor.CompletionCount) + ' lines');
      Check('OCL editor: the evaluator''s variables', Editor.VariableCount >= 2, IntToStr(Editor.VariableCount));
      Editor.Expression := 'Customer.allInstances';
      Check('OCL editor: a valid expression and its type', Editor.LastCheck.Valid and
        (Editor.StatusText = 'Syntax is OK' + sLineBreak + sLineBreak + 'Current type is: Collection(Customer)'),
        OneLine(Editor.StatusText));
      Check('OCL editor: the memo shows the expression',
        BoldOclTrimLineBreaks(Editor.ExpressionMemo.Text) = 'Customer.allInstances', Editor.ExpressionMemo.Text);
      Index := IndexOfCompletion(Editor, '.invoices');
      Check('OCL editor: a multi link of the elements', (Index >= 0) and
        (Editor.Completions[Index].Kind = ockMultiLink) and (Editor.Completions[Index].Detail = ': List<Invoice>'),
        IntToStr(Index));
      Editor.SetFilter(ocfOperations);
      Index := IndexOfCompletion(Editor, '->select(«Boolean»)');
      Check('OCL editor: an operation with its argument', Index >= 0, IntToStr(Editor.CompletionCount) + ' operations');
      Editor.AddCompletion(Index);
      Check('OCL editor: the operation is added', (Editor.Expression = cSelect) and not Editor.LastCheck.Valid and
        (BoldOclTrimLineBreaks(Editor.ExpressionMemo.Text) = cSelect), Editor.ExpressionMemo.Text);
      // The caret inside the placeholder, as after a click there
      Editor.ExpressionMemo.SelStart := Length('Customer.allInstances->select(') + 3;
      Editor.ExpressionMemo.SelLength := 0;
      Editor.SelectPlaceholder;
      Check('OCL editor: a click in a placeholder selects it', Editor.ExpressionMemo.SelText = '«Boolean»',
        Editor.ExpressionMemo.SelText);
      // The same in the second line: the memo counts its line breaks as the text has them
      Editor.Expression := 'Customer.allInstances' + sLineBreak + '->select(«Boolean»)';
      Editor.ExpressionMemo.SelStart := Length('Customer.allInstances' + sLineBreak + '->select(') + 3;
      Editor.ExpressionMemo.SelLength := 0;
      Editor.SelectPlaceholder;
      Check('OCL editor: a placeholder in the second line', Editor.ExpressionMemo.SelText = '«Boolean»',
        Editor.ExpressionMemo.SelText);
      Editor.RemoveLast;
      Check('OCL editor: Remove Last', Editor.Expression = 'Customer.allInstances', OneLine(Editor.Expression));
      Editor.SetFilter(ocfText, 'INVOICED');
      Index := IndexOfCompletion(Editor, '.invoicedTotal');
      Check('OCL editor: a typed filter', (Index >= 0) and (Editor.CompletionCount < 5),
        IntToStr(Editor.CompletionCount) + ' lines');
      Editor.AddCompletion(Index);
      Check('OCL editor: after an addition the typed filter gives way',
        (Editor.Expression = 'Customer.allInstances.invoicedTotal') and (Editor.State.KindFilter = ocfAll),
        Editor.Expression);
      Editor.Expression := 'Customer.allInstances.nosuch';
      Check('OCL editor: an error, with a pointer at it', not Editor.LastCheck.Valid and
        (Editor.CompletionCount = 0) and (Pos('^', Editor.StatusText) > 0), OneLine(Editor.StatusText));
      Editor.ClearExpression;
      Check('OCL editor: Clear', (Editor.Expression = '') and (Editor.StatusText = 'Empty Expression'),
        OneLine(Editor.StatusText));
    finally
      Editor.Free;
    end;
  end;

  function ShownInTheComboBox: string;
  begin
    if Assigned(FInvoiceCustomer.Selected) then
      Result := FInvoiceCustomer.Selected.Text
    else
      Result := '';
  end;

  { Picks an item of the combo box as the control does for a user: it asks
    its LiveBindings observers whether the field may be edited (which puts
    the dataset in edit mode), changes its selection, and tells them that
    it has a new value to write. }
  procedure PickInTheComboBox(AIndex: Integer);
  var
    Observers: TObservers;
  begin
    Observers := FInvoiceCustomer.Observers;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
    begin
      if TLinkObservers.EditLinkEdit(Observers) then
      begin
        FInvoiceCustomer.ItemIndex := AIndex;
        TLinkObservers.EditLinkModified(Observers);
        TLinkObservers.EditLinkUpdate(Observers);
      end
      else
        Log.Add('  the combo box may not edit the field');
    end
    else
    begin
      FInvoiceCustomer.ItemIndex := AIndex;
      TLinkObservers.ControlChanged(FInvoiceCustomer);
    end;
    Settle;
  end;

  { The combo box under the invoice grid: a single link through datasets }
  procedure CheckTheCustomerComboBox;
  var
    Customer, OtherCustomer: TCustomer;
    Invoice: TInvoice;
    InvoicesBefore, Choice: Integer;
    IdleDone: Boolean;
  begin
    FCustomers.First;
    while not FCustomers.Eof and (FInvoices.RecordCount = 0) do
      FCustomers.Next;
    Settle;
    Customer := FCustomers.ActiveBoldObject as TCustomer;
    FInvoices.First;
    Settle;
    Invoice := FInvoices.ActiveBoldObject as TInvoice;
    InvoicesBefore := Customer.Invoices.Count;
    Check('the invoice grid has no column for the customer key', FInvoiceGrid.ColumnCount = 6,
      IntToStr(FInvoiceGrid.ColumnCount) + ' columns');
    Check('the combo box lists every customer',
      FInvoiceCustomer.Count = FData.BoldSystem.EvaluateExpressionAsInteger('Customer.allInstances->size'),
      IntToStr(FInvoiceCustomer.Count));
    Check('the combo box shows the customer of the selected invoice', ShownInTheComboBox = Customer.Name,
      ShownInTheComboBox);
    OtherCustomer := nil;
    Choice := 0;
    while (Choice < FInvoiceCustomer.Count) and (FInvoiceCustomer.ListItems[Choice].Text = Customer.Name) do
      Inc(Choice);
    PickInTheComboBox(Choice);
    if Assigned(Invoice.Customer) then
      OtherCustomer := Invoice.Customer;
    Check('picking another customer in the combo box links the invoice to it',
      Assigned(OtherCustomer) and (OtherCustomer <> Customer),
      'invoice ' + IntToStr(Invoice.Number) + ' of ' + Invoice.Customer.Name);
    Check('the invoice is no longer among the invoices of its former customer',
      Customer.Invoices.Count = InvoicesBefore - 1, IntToStr(Customer.Invoices.Count) + ' invoices');
    Check('the invoice dataset is not left editing', not (FInvoices.State in dsEditModes),
      IntToStr(Ord(FInvoices.State)));
    Check('the customer grid has moved to the new customer', FCustomers.ActiveBoldObject = OtherCustomer,
      FCustomers.FieldByName('Name').AsString);
    Check('the invoice grid shows that customer''s invoices, with the invoice selected',
      (FInvoices.ActiveBoldObject = Invoice) and Assigned(OtherCustomer) and
      (FInvoiceGrid.RowCount = OtherCustomer.Invoices.Count), IntToStr(FInvoiceGrid.RowCount) + ' rows');
    Check('the combo box shows the new customer and can be used again',
      FInvoiceCustomer.Enabled and Assigned(OtherCustomer) and (ShownInTheComboBox = OtherCustomer.Name),
      ShownInTheComboBox);
    if Customer.Invoices.Count = 0 then
    begin
      FCustomers.Locate('Number', Customer.Number, []);
      Settle;
      // What the idle loop does, where the combo box is enabled or not
      Application.DoIdle(IdleDone);
      Check('a customer without invoices: an empty invoice grid, and the combo box cannot be used',
        (FCustomers.ActiveBoldObject = Customer) and (FInvoiceGrid.RowCount = 0) and not FInvoiceCustomer.Enabled,
        Format('%d rows, enabled %s', [FInvoiceGrid.RowCount, BoolToStr(FInvoiceCustomer.Enabled, True)]));
      if Assigned(OtherCustomer) then
        FCustomers.Locate('Number', OtherCustomer.Number, []);
      Settle;
      Application.DoIdle(IdleDone);
    end;
    Choice := 0;
    while (Choice < FInvoiceCustomer.Count) and (FInvoiceCustomer.ListItems[Choice].Text <> Customer.Name) do
      Inc(Choice);
    PickInTheComboBox(Choice);
    Check('picking the former customer brings the invoice back to it',
      (Invoice.Customer = Customer) and (Customer.Invoices.Count = InvoicesBefore),
      'invoice ' + IntToStr(Invoice.Number) + ' of ' + Invoice.Customer.Name);
    Check('and the form follows it there',
      (FCustomers.ActiveBoldObject = Customer) and (FInvoices.ActiveBoldObject = Invoice) and
      (ShownInTheComboBox = Customer.Name), FCustomers.FieldByName('Name').AsString + ', ' + ShownInTheComboBox);
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

    CheckTheCustomerComboBox;

    // Only an attribute changes, on the Bold side; the change is not saved.
    // TBoldDataSet's cell followers hear it, and LiveBindings fill the cell.
    FCustomers.First;
    Settle;
    Customer := FCustomers.ActiveBoldObject as TCustomer;
    Customer.Name := Customer.Name + ' (renamed)';
    Settle;
    Check('a rename on the Bold side shows in the grid',
      FCustomerGrid.Cells[1, FCustomers.RecNo - 1] = Customer.Name, FCustomerGrid.Cells[1, FCustomers.RecNo - 1]);
    CheckTheOclEditor;
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
