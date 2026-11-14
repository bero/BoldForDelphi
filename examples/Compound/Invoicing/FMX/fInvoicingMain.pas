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

  All of that is in the form file (fInvoicingMain.fmx): the list handles, the
  datasets with their fields, the bind sources, the bindings and the controls
  are components on the form, wired through their properties, as the form
  designer writes them with the design package dclBold installed. The
  Lazarus client (../LCL) has the same form. The model, the system handle and
  the database are in the data module the console runner and the tests use
  (../Data/dmInvoicing.pas), which the code creates; so the code roots the
  two top list handles on its system handle, and opens the datasets.

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
  FMX.Graphics,
  FMX.Objects,
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
    blhCustomers: TBoldListHandle;
    blhCustomerChoice: TBoldListHandle;
    blhInvoices: TBoldListHandle;
    blhLines: TBoldListHandle;
    bdsCustomers: TBoldDataSet;
    bdsCustomerChoice: TBoldDataSet;
    bdsInvoices: TBoldDataSet;
    bdsLines: TBoldDataSet;
    bsCustomers: TBindSourceDB;
    bsCustomerChoice: TBindSourceDB;
    bsInvoices: TBindSourceDB;
    bsLines: TBindSourceDB;
    BindingsList1: TBindingsList;
    lnkCustomers: TLinkGridToDataSource;
    lnkInvoices: TLinkGridToDataSource;
    lnkLines: TLinkGridToDataSource;
    lnkInvoiceCustomer: TLinkFillControlToField;
    lblCustomers: TLabel;
    grdCustomers: TStringGrid;
    lblInvoices: TLabel;
    grdInvoices: TStringGrid;
    layInvoice: TLayout;
    lblInvoiceCustomer: TLabel;
    cmbInvoiceCustomer: TComboBox;
    btnNewInvoice: TButton;
    btnPostInvoice: TButton;
    layLines: TLayout;
    lblLines: TLabel;
    btnAddLine: TButton;
    btnDeleteLine: TButton;
    grdLines: TStringGrid;
    layBar: TLayout;
    lblStatus: TLabel;
    btnEditOcl: TButton;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure EditOclClick(Sender: TObject);
    procedure InvoiceCustomerChange(Sender: TObject);
    procedure NewInvoiceClick(Sender: TObject);
    procedure PostInvoiceClick(Sender: TObject);
    procedure AddLineClick(Sender: TObject);
    procedure DeleteLineClick(Sender: TObject);
    procedure GridDrawColumnBackground(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
      const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn;
      const Bounds: TRectF; const Row: Integer; const Value: TValue; const State: TGridDrawStates);
  private
    FData: TdmInvoicing;
    FDatabase: string;
    FSelfTest: Boolean;
    FSelfTestException: Boolean;
    FFollowQueued: Boolean;
    // A TInvoice; the model's classes are not named in the interface
    FInvoiceToFollow: TObject;
    { What Edit OCL opens with: the expression of the last OK }
    FOclExpression: string;
    procedure FollowTheInvoice;
    procedure ApplyTheLook;
    procedure UpdateInvoiceControls;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
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

  // The handles are chained in the form file: the invoices on the customers,
  // the lines on the invoices. The system handle is in the data module, so
  // the two lists of all customers get their root here.
  blhCustomers.StaticSystemHandle := FData.BoldSystemHandle1;
  blhCustomers.RootHandle := FData.BoldSystemHandle1;
  blhCustomerChoice.StaticSystemHandle := FData.BoldSystemHandle1;
  blhCustomerChoice.RootHandle := FData.BoldSystemHandle1;
  blhInvoices.StaticSystemHandle := FData.BoldSystemHandle1;
  blhLines.StaticSystemHandle := FData.BoldSystemHandle1;
  // The datasets open on their handles, and the grids and the combo box
  // fill through their bindings
  bdsCustomers.Open;
  bdsCustomerChoice.Open;
  bdsInvoices.Open;
  bdsLines.Open;

  ApplyTheLook;
  FOclExpression := 'Customer.allInstances';
  Application.OnIdle := AppIdle;
  ShowStatus;
  SelfTestTrace('grids bound');
end;

procedure TfrmInvoicingMain.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  FInvoiceToFollow := nil;
  // Details before masters, datasets before their handles, all before the system
  FreeAndNil(bdsLines);
  FreeAndNil(bdsInvoices);
  FreeAndNil(bdsCustomerChoice);
  FreeAndNil(bdsCustomers);
  FreeAndNil(blhLines);
  FreeAndNil(blhInvoices);
  FreeAndNil(blhCustomerChoice);
  FreeAndNil(blhCustomers);
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

procedure TfrmInvoicingMain.InvoiceCustomerChange(Sender: TObject);
begin
  // A pick, or the invoice grid moved to another invoice. After a pick the
  // invoice is about to leave the invoice grid, which shows the invoices of
  // the selected customer: the form follows the invoice, once the combo box
  // and its link are done with the pick.
  // The invoice is noted now: once the pick is written it is no longer the
  // selected one, and the combo box changes again, for the invoice that is
  // selected then. Only the first change counts until the form has followed.
  if not FFollowQueued then
  begin
    if Assigned(bdsInvoices) and bdsInvoices.Active and not bdsInvoices.IsEmpty then
      FInvoiceToFollow := bdsInvoices.ActiveBoldObject
    else
      FInvoiceToFollow := nil;
    FFollowQueued := True;
    TThread.ForceQueue(nil, FollowTheInvoice);
  end;
end;

procedure TfrmInvoicingMain.FollowTheInvoice;
var
  Invoice: TInvoice;
begin
  FFollowQueued := False;
  if not Assigned(bdsInvoices) or not Assigned(bdsCustomers) or not (FInvoiceToFollow is TInvoice) then
    Exit;
  Invoice := TInvoice(FInvoiceToFollow);
  FInvoiceToFollow := nil;
  if not Assigned(Invoice.Customer) or (bdsCustomers.ActiveBoldObject = Invoice.Customer) then
    Exit;
  // The datasets follow a change when Bold's display queue is drained, which
  // AppIdle does; here it is drained at once, so that the invoice dataset
  // has let the invoice go before the customer grid moves
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

{ The look of the LCL client (../LCL), which FMX's default style does not
  have: its font size, in the form file for the labels, buttons and grid
  cells and here for what is made while the program runs (the column headers
  and the items of the combo box), and its selection, a blue row with white
  text. FMX's grid shows the selected row with a control of its style laid
  over the cells; that one is made invisible, and the two grid events below
  draw the cells. Called from the idle loop: the style's parts are made when
  a control is first shown, and the items when the list is filled. }

const
  cFontSize = 15;
  cScrollBarColor = $FFB4B4B4;
  cOurThumb = 'BoldLookThumb';
  cSelectionColor = $FF308CC6;
  cSelectionTextColor = TAlphaColors.White;
  cTextColor = TAlphaColors.Black;

procedure TfrmInvoicingMain.ApplyTheLook;

  procedure HideTheStyleSelection(AObject: TFmxObject);
  var
    i: Integer;
  begin
    // 'focus' is the frame around the current cell, which a row that is
    // selected as a whole does not need
    if (AObject is TControl) and
      (SameText(AObject.StyleName, 'selection') or SameText(AObject.StyleName, 'focus')) then
      TControl(AObject).Opacity := 0;
    for i := 0 to AObject.ChildrenCount - 1 do
      HideTheStyleSelection(AObject.Children[i]);
  end;

  { The thumb of a scroll bar is a picture of the style, in its accent
    colour; it is made invisible and a grey rectangle takes its place }
  procedure GreyScrollBars(AObject: TFmxObject);
  var
    i: Integer;
    HasOurs: Boolean;
    Ours: TRectangle;
  begin
    if AObject is TThumb then
    begin
      HasOurs := False;
      for i := 0 to AObject.ChildrenCount - 1 do
        if AObject.Children[i].TagString = cOurThumb then
          HasOurs := True
        else if AObject.Children[i] is TControl then
          TControl(AObject.Children[i]).Opacity := 0;
      if not HasOurs then
      begin
        Ours := TRectangle.Create(AObject);
        Ours.TagString := cOurThumb;
        Ours.Stored := False;
        Ours.HitTest := False;
        Ours.Align := TAlignLayout.Contents;
        Ours.Stroke.Kind := TBrushKind.None;
        Ours.Fill.Color := cScrollBarColor;
        Ours.XRadius := 3;
        Ours.YRadius := 3;
        Ours.Parent := AObject;
      end;
      Exit;
    end;
    for i := 0 to AObject.ChildrenCount - 1 do
      GreyScrollBars(AObject.Children[i]);
  end;

  procedure LookOfGrid(AGrid: TStringGrid);
  var
    i: Integer;
  begin
    for i := 0 to AGrid.ColumnCount - 1 do
      if AGrid.Columns[i].HeaderSettings.TextSettings.Font.Size <> cFontSize then
      begin
        AGrid.Columns[i].HeaderSettings.StyledSettings :=
          AGrid.Columns[i].HeaderSettings.StyledSettings - [TStyledSetting.Size];
        AGrid.Columns[i].HeaderSettings.TextSettings.Font.Size := cFontSize;
      end;
    HideTheStyleSelection(AGrid);
    GreyScrollBars(AGrid);
  end;

var
  i: Integer;
begin
  LookOfGrid(grdCustomers);
  LookOfGrid(grdInvoices);
  LookOfGrid(grdLines);
  for i := 0 to cmbInvoiceCustomer.Count - 1 do
    if cmbInvoiceCustomer.ListItems[i].TextSettings.Font.Size <> cFontSize then
    begin
      cmbInvoiceCustomer.ListItems[i].StyledSettings :=
        cmbInvoiceCustomer.ListItems[i].StyledSettings - [TStyledSetting.Size];
      cmbInvoiceCustomer.ListItems[i].TextSettings.Font.Size := cFontSize;
    end;
end;

procedure TfrmInvoicingMain.GridDrawColumnBackground(Sender: TObject; const Canvas: TCanvas;
  const Column: TColumn; const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
begin
  if Row = (Sender as TStringGrid).Row then
  begin
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := cSelectionColor;
    Canvas.FillRect(Bounds, 0, 0, [], 1);
  end;
end;

procedure TfrmInvoicingMain.GridDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
  const Column: TColumn; const Bounds: TRectF; const Row: Integer; const Value: TValue;
  const State: TGridDrawStates);
var
  TextRect: TRectF;
begin
  TextRect := Bounds;
  TextRect.Inflate(-4, 0);
  Canvas.Font.Assign((Sender as TStringGrid).TextSettings.Font);
  Canvas.Fill.Kind := TBrushKind.Solid;
  if Row = TStringGrid(Sender).Row then
    Canvas.Fill.Color := cSelectionTextColor
  else
    Canvas.Fill.Color := cTextColor;
  Canvas.FillText(TextRect, Value.ToString, False, 1, [], Column.HorzAlign, TTextAlign.Center);
end;

procedure TfrmInvoicingMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  // Brings the Bold-aware followers, TBoldDataSet's among them, up to date
  TBoldQueueable.DisplayAll;
  ApplyTheLook;
  UpdateInvoiceControls;
  ShowStatus;
  Done := True;
end;

{ What can be done with the selected invoice; called from the idle loop }
procedure TfrmInvoicingMain.UpdateInvoiceControls;
var
  HasInvoice, IsDraft: Boolean;
begin
  if not Assigned(bdsInvoices) or not Assigned(bdsCustomers) then
    Exit;
  HasInvoice := bdsInvoices.Active and not bdsInvoices.IsEmpty and (bdsInvoices.ActiveBoldObject is TInvoice);
  IsDraft := HasInvoice and not TInvoice(bdsInvoices.ActiveBoldObject).IsPosted;
  // A posted invoice is frozen, which the model says in its business methods
  // (TInvoice.AddLine); the form follows that rule here. So only a draft
  // gets another customer, and without an invoice there is nothing to pick
  // a customer for.
  cmbInvoiceCustomer.Enabled := IsDraft;
  // The lines of a draft are edited in the grid
  grdLines.ReadOnly := not IsDraft;
  btnAddLine.Enabled := IsDraft;
  btnDeleteLine.Enabled := IsDraft and bdsLines.Active and not bdsLines.IsEmpty;
  btnPostInvoice.Enabled := IsDraft;
  btnNewInvoice.Enabled := bdsCustomers.Active and not bdsCustomers.IsEmpty;
end;

procedure TfrmInvoicingMain.NewInvoiceClick(Sender: TObject);
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
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.AddLineClick(Sender: TObject);
begin
  // A new row at the end of the lines grid, ready to be typed in: an FMX
  // grid has no row to start in while it is empty. The dataset asks the
  // list of its handle, the lines of the selected invoice, for the new
  // object, so it is a line of that invoice at once.
  bdsLines.Append;
  if Visible and grdLines.CanFocus then
  begin
    grdLines.SetFocus;
    grdLines.Col := 0;
    grdLines.EditorMode := True;
  end;
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.DeleteLineClick(Sender: TObject);
begin
  if not bdsLines.IsEmpty then
    bdsLines.Delete;
  UpdateInvoiceControls;
end;

procedure TfrmInvoicingMain.PostInvoiceClick(Sender: TObject);
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
  if Assigned(lblStatus) and Assigned(FData) and FData.IsOpen then
    lblStatus.Text := Format('%s  -  %d customers, %d invoices%s', [FData.Database,
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

  { A new invoice, its lines edited as the grid edits them (through the
    dataset of the grid), and posted }
  procedure CheckEditingTheLines;
  var
    Customer: TCustomer;
    Invoice: TInvoice;
    Line: TInvoiceLine;
    InvoicesBefore: Integer;
    Refused: string;
    IdleDone: Boolean;
  begin
    bdsCustomers.First;
    Settle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    bdsInvoices.First;
    Settle;
    Application.DoIdle(IdleDone);
    if bdsInvoices.ActiveBoldObject is TInvoice then
      Check('the lines of a posted invoice cannot be edited in the grid',
        TInvoice(bdsInvoices.ActiveBoldObject).IsPosted and grdLines.ReadOnly and not btnPostInvoice.Enabled,
        'read-only ' + BoolToStr(grdLines.ReadOnly, True));

    InvoicesBefore := Customer.Invoices.Count;
    NewInvoiceClick(btnNewInvoice);
    Settle;
    Check('New invoice makes a draft for the selected customer, and selects it',
      (Customer.Invoices.Count = InvoicesBefore + 1) and (bdsInvoices.ActiveBoldObject is TInvoice) and
      not TInvoice(bdsInvoices.ActiveBoldObject).IsPosted and
      (TInvoice(bdsInvoices.ActiveBoldObject).Customer = Customer),
      Format('%d invoices, status %s', [Customer.Invoices.Count, bdsInvoices.FieldByName('Status').AsString]));
    if not (bdsInvoices.ActiveBoldObject is TInvoice) then
      Exit;
    Invoice := TInvoice(bdsInvoices.ActiveBoldObject);
    Check('the lines of a draft can be edited in the grid, and it can be posted',
      not grdLines.ReadOnly and btnPostInvoice.Enabled and (bdsLines.RecordCount = 0) and (grdLines.RowCount = 0),
      Format('read-only %s, %d lines', [BoolToStr(grdLines.ReadOnly, True), bdsLines.RecordCount]));

    Refused := '';
    try
      PostInvoiceClick(btnPostInvoice);
    except
      on E: Exception do
        Refused := E.Message;
    end;
    Check('an invoice without lines is not posted', (Refused <> '') and not Invoice.IsPosted, Refused);

    // The idle loop, where the buttons are enabled or not
    Settle;
    Application.DoIdle(IdleDone);
    Check('a draft without lines: Add line can be used, Delete line cannot',
      btnAddLine.Enabled and not btnDeleteLine.Enabled, BoolToStr(btnAddLine.Enabled, True));
    AddLineClick(btnAddLine);
    Settle;
    Check('Add line gives the lines grid a new row to type in',
      (bdsLines.State = dsInsert) and (grdLines.RowCount = 1) and (Invoice.Lines.Count = 1),
      IntToStr(grdLines.RowCount) + ' rows, state ' + IntToStr(Ord(bdsLines.State)));
    bdsLines.FieldByName('Description').AsString := 'Workshop';
    bdsLines.FieldByName('Quantity').AsFloat := 2;
    bdsLines.FieldByName('Unit price').AsCurrency := 100;
    bdsLines.FieldByName('VAT %').AsFloat := 25;
    bdsLines.Post;
    Settle;
    Check('a line typed in the grid is a line of the invoice',
      (Invoice.Lines.Count = 1) and (grdLines.RowCount = 1) and (Invoice.Lines[0].Description = 'Workshop') and
      (grdLines.Cells[0, 0] = 'Workshop'), Format('%d lines, %d rows', [Invoice.Lines.Count, grdLines.RowCount]));
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

    bdsLines.Edit;
    bdsLines.FieldByName('Quantity').AsFloat := 99;
    bdsLines.Cancel;
    Settle;
    Check('Cancel takes back what was typed, and keeps the line',
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
      (grdLines.RowCount = (bdsInvoices.ActiveBoldObject as TInvoice).Lines.Count) and
      (bdsLines.ActiveBoldObject is TInvoiceLine) and
      (TInvoiceLine(bdsLines.ActiveBoldObject).Invoice = bdsInvoices.ActiveBoldObject),
      IntToStr(grdLines.RowCount) + ' rows, state ' + IntToStr(Ord(bdsLines.State)));
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
    Check('a second line', (Invoice.Lines.Count = 2) and (grdLines.RowCount = 2), IntToStr(Invoice.Lines.Count));
    Line := bdsLines.ActiveBoldObject as TInvoiceLine;
    DeleteLineClick(btnDeleteLine);
    Settle;
    Check('a deleted row is a deleted line',
      (Invoice.Lines.Count = 1) and (grdLines.RowCount = 1) and (Invoice.Lines[0] <> Line) and
      (bdsInvoices.FieldByName('Total').AsCurrency = 375),
      Format('%d lines, total %s', [Invoice.Lines.Count, bdsInvoices.FieldByName('Total').AsString]));

    PostInvoiceClick(btnPostInvoice);
    Settle;
    Check('Post invoice posts it: a number, and the status of an open invoice',
      Invoice.IsPosted and (Invoice.Number > 0) and (bdsInvoices.ActiveBoldObject = Invoice) and
      (bdsInvoices.FieldByName('Status').AsString = Invoice.Status),
      Format('%d, %s', [Invoice.Number, bdsInvoices.FieldByName('Status').AsString]));
    Check('and its lines can no longer be edited in the grid', grdLines.ReadOnly and not btnPostInvoice.Enabled,
      'read-only ' + BoolToStr(grdLines.ReadOnly, True));
  end;

  function ShownInTheComboBox: string;
  begin
    if Assigned(cmbInvoiceCustomer.Selected) then
      Result := cmbInvoiceCustomer.Selected.Text
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
    Observers := cmbInvoiceCustomer.Observers;
    if Observers.IsObserving(TObserverMapping.EditLinkID) then
    begin
      if TLinkObservers.EditLinkEdit(Observers) then
      begin
        cmbInvoiceCustomer.ItemIndex := AIndex;
        TLinkObservers.EditLinkModified(Observers);
        TLinkObservers.EditLinkUpdate(Observers);
      end
      else
        Log.Add('  the combo box may not edit the field');
    end
    else
    begin
      cmbInvoiceCustomer.ItemIndex := AIndex;
      TLinkObservers.ControlChanged(cmbInvoiceCustomer);
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

    // What the idle loop does, where the controls are enabled or not
    procedure Idle;
    begin
      Settle;
      Application.DoIdle(IdleDone);
    end;

  begin
    bdsCustomers.First;
    while not bdsCustomers.Eof and (bdsInvoices.RecordCount > 0) do
      bdsCustomers.Next;
    Idle;
    if not bdsCustomers.Eof then
      Check('a customer without invoices: an empty invoice grid, and the combo box cannot be used',
        (grdInvoices.RowCount = 0) and not cmbInvoiceCustomer.Enabled,
        Format('%d rows, enabled %s', [grdInvoices.RowCount, BoolToStr(cmbInvoiceCustomer.Enabled, True)]));
    bdsCustomers.First;
    while not bdsCustomers.Eof and (bdsInvoices.RecordCount = 0) do
      bdsCustomers.Next;
    Idle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    bdsInvoices.First;
    Idle;
    Check('the invoice grid has no column for the customer key', grdInvoices.ColumnCount = 6,
      IntToStr(grdInvoices.ColumnCount) + ' columns');
    Check('the combo box lists every customer',
      cmbInvoiceCustomer.Count = FData.BoldSystem.EvaluateExpressionAsInteger('Customer.allInstances->size'),
      IntToStr(cmbInvoiceCustomer.Count));
    Check('the combo box shows the customer of the selected invoice', ShownInTheComboBox = Customer.Name,
      ShownInTheComboBox);
    Check('a posted invoice is frozen: it cannot get another customer',
      (bdsInvoices.ActiveBoldObject as TInvoice).IsPosted and not cmbInvoiceCustomer.Enabled,
      'enabled ' + BoolToStr(cmbInvoiceCustomer.Enabled, True));

    // A draft gets another customer
    NewInvoiceClick(btnNewInvoice);
    Idle;
    Invoice := bdsInvoices.ActiveBoldObject as TInvoice;
    InvoicesBefore := Customer.Invoices.Count;
    Check('for a draft the combo box can be used, and shows its customer',
      not Invoice.IsPosted and cmbInvoiceCustomer.Enabled and (ShownInTheComboBox = Customer.Name),
      ShownInTheComboBox);
    OtherCustomer := nil;
    Choice := 0;
    while (Choice < cmbInvoiceCustomer.Count) and (cmbInvoiceCustomer.ListItems[Choice].Text = Customer.Name) do
      Inc(Choice);
    PickInTheComboBox(Choice);
    Idle;
    if Assigned(Invoice.Customer) then
      OtherCustomer := Invoice.Customer;
    Check('picking another customer in the combo box links the invoice to it',
      Assigned(OtherCustomer) and (OtherCustomer <> Customer), 'the draft of ' + Invoice.Customer.Name);
    Check('the invoice is no longer among the invoices of its former customer',
      Customer.Invoices.Count = InvoicesBefore - 1, IntToStr(Customer.Invoices.Count) + ' invoices');
    Check('the invoice dataset is not left editing', not (bdsInvoices.State in dsEditModes),
      IntToStr(Ord(bdsInvoices.State)));
    Check('the customer grid has moved to the new customer', bdsCustomers.ActiveBoldObject = OtherCustomer,
      bdsCustomers.FieldByName('Name').AsString);
    Check('the invoice grid shows that customer''s invoices, with the invoice selected',
      (bdsInvoices.ActiveBoldObject = Invoice) and Assigned(OtherCustomer) and
      (grdInvoices.RowCount = OtherCustomer.Invoices.Count), IntToStr(grdInvoices.RowCount) + ' rows');
    Check('the combo box shows the new customer and can be used again',
      cmbInvoiceCustomer.Enabled and Assigned(OtherCustomer) and (ShownInTheComboBox = OtherCustomer.Name),
      ShownInTheComboBox);
    Choice := 0;
    while (Choice < cmbInvoiceCustomer.Count) and (cmbInvoiceCustomer.ListItems[Choice].Text <> Customer.Name) do
      Inc(Choice);
    PickInTheComboBox(Choice);
    Idle;
    Check('picking the former customer brings the invoice back to it',
      (Invoice.Customer = Customer) and (Customer.Invoices.Count = InvoicesBefore),
      'the draft of ' + Invoice.Customer.Name);
    Check('and the form follows it there',
      (bdsCustomers.ActiveBoldObject = Customer) and (bdsInvoices.ActiveBoldObject = Invoice) and
      (ShownInTheComboBox = Customer.Name), bdsCustomers.FieldByName('Name').AsString + ', ' + ShownInTheComboBox);
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
    Check('customer rows', grdCustomers.RowCount = bdsCustomers.RecordCount,
      Format('%d rows, %d customers', [grdCustomers.RowCount, bdsCustomers.RecordCount]));
    Check('customers', bdsCustomers.RecordCount > 0, IntToStr(bdsCustomers.RecordCount));
    InvoicesShown := 0;
    LinesShown := 0;
    bdsCustomers.First;
    while not bdsCustomers.Eof do
    begin
      Settle;
      Customer := bdsCustomers.ActiveBoldObject as TCustomer;
      Check('name of customer ' + IntToStr(Customer.Number) + ' in its row',
        grdCustomers.Cells[1, bdsCustomers.RecNo - 1] = Customer.Name, grdCustomers.Cells[1, bdsCustomers.RecNo - 1]);
      Check('invoice rows of customer ' + IntToStr(Customer.Number),
        grdInvoices.RowCount = Customer.Invoices.Count,
        Format('%d rows, %d invoices', [grdInvoices.RowCount, Customer.Invoices.Count]));
      Inc(InvoicesShown, grdInvoices.RowCount);
      bdsInvoices.First;
      while not bdsInvoices.Eof do
      begin
        Settle;
        Invoice := bdsInvoices.ActiveBoldObject as TInvoice;
        Check('line rows of invoice ' + IntToStr(Invoice.Number),
          grdLines.RowCount = Invoice.Lines.Count,
          Format('%d rows, %d lines', [grdLines.RowCount, Invoice.Lines.Count]));
        Inc(LinesShown, grdLines.RowCount);
        bdsInvoices.Next;
      end;
      bdsCustomers.Next;
    end;
    Check('invoices shown over all customers',
      InvoicesShown = FData.BoldSystem.EvaluateExpressionAsInteger('Invoice.allInstances->size'), IntToStr(InvoicesShown));
    Check('some lines shown', LinesShown > 0, IntToStr(LinesShown));

    CheckTheCustomerComboBox;

    // Only an attribute changes, on the Bold side; the change is not saved.
    // TBoldDataSet's cell followers hear it, and LiveBindings fill the cell.
    bdsCustomers.First;
    Settle;
    Customer := bdsCustomers.ActiveBoldObject as TCustomer;
    Customer.Name := Customer.Name + ' (renamed)';
    Settle;
    Check('a rename on the Bold side shows in the grid',
      grdCustomers.Cells[1, bdsCustomers.RecNo - 1] = Customer.Name, grdCustomers.Cells[1, bdsCustomers.RecNo - 1]);
    CheckTheOclEditor;
    CheckEditingTheLines;
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
