{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionDialogFMX;

{ Delphi's OCL editor as an FMX dialog (#124), for Windows, macOS and Linux:
  the dialog of the Lazarus property editor BoldOclExpressionPropEdit, over
  the same TBoldOclEditorState (BoldOclExpressionCheck). The state checks the
  expression, makes the lists and applies the filter; this unit shows it.

  In the middle the expression and, under it, the parser messages: the type
  of the result, or the expression with a pointer at the error. At the right
  what can follow the expression, with a filter and the Derived, Persistent
  and Transient boxes; a double click or Enter adds the line to the
  expression. At the left the OCL variables.

  Not here from the Delphi editor: the font button and the list of recently
  used expressions, which are kept in the Windows registry. }

interface

uses
  System.Classes,
  System.UITypes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Memo,
  FMX.Memo.Types,
  FMX.ListBox,
  FMX.ComboEdit,
  FMX.Menus,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  BoldElements,
  BoldOclExpressionCheck;

type
  TBoldOclExpressionDialogFMX = class(TForm)
  private
    fState: TBoldOclEditorState;
    fExpressionMemo: TMemo;
    fMessagesMemo: TMemo;
    fSyntaxCheckBox: TCheckBox;
    fFilterCombo: TComboEdit;
    fCompletionList: TListBox;
    fVariableList: TListBox;
    fVariablePanel: TLayout;
    fVariableSplitter: TSplitter;
    fDerivedCheckBox, fPersistentCheckBox, fTransientCheckBox: TCheckBox;
    fModelErrorsButton: TButton;
    fIcons: array[TBoldOclCompletionKind] of FMX.Graphics.TBitmap;
    { While the dialog sets its controls from fState, their events do nothing }
    fUpdating: Boolean;
    function GetExpression: string;
    function GetLastCheck: TBoldOclCheckResult;
    function GetStatusText: string;
    function GetCompletionCount: Integer;
    function GetCompletion(AIndex: Integer): TBoldOclCompletion;
    function GetVariableCount: Integer;
    function GetVariable(AIndex: Integer): TBoldOclCompletion;
    function GetMemberOptions: TBoldOclMemberOptions;
    procedure SetMemberOptions(AValue: TBoldOclMemberOptions);
    function GetShowSyntaxErrors: Boolean;
    procedure SetShowSyntaxErrors(AValue: Boolean);
    procedure SetExpression(const AValue: string);
    procedure LoadIcons;
    procedure BuildControls;
    procedure ShowState(Sender: TObject);
    procedure FillList(AList: TListBox; AVariables: Boolean);
    procedure ExpressionChanged(Sender: TObject);
    procedure ExpressionKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ExpressionMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OptionChanged(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FilterExit(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ClearClicked(Sender: TObject);
    procedure RemoveLastClicked(Sender: TObject);
    procedure ShowInfoClicked(Sender: TObject);
    procedure ModelErrorsClicked(Sender: TObject);
    procedure CopyMessagesClicked(Sender: TObject);
  public
    { AContext may be nil: the expression can be edited but is not checked.
      AEvaluator: another evaluator than the one of the context's system. }
    constructor CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
      AVariables: TBoldExternalVariableList; AEvaluator: TBoldEvaluator = nil);
    destructor Destroy; override;
    { The filter of the list at the right: a line of its list (ocfAll ..
      ocfOperations), or with ocfText a text the lines must contain }
    procedure SetFilter(AKindFilter: Integer; const AText: string = '');
    { What a double click on a line does: adds it to the expression }
    procedure AddCompletion(AIndex: Integer);
    procedure AddVariable(AIndex: Integer);
    procedure RemoveLast;
    procedure ClearExpression;
    { Selects the argument placeholder «...» around the caret of the
      expression, as a click or a key there does }
    procedure SelectPlaceholder;
    { The icons of the lists, one for each kind of line }
    function IconCount: Integer;
    { The editor without its controls, the same as in the Lazarus dialog }
    property State: TBoldOclEditorState read fState;
    property ExpressionMemo: TMemo read fExpressionMemo;
    property Expression: string read GetExpression write SetExpression;
    property LastCheck: TBoldOclCheckResult read GetLastCheck;
    { The parser messages under the expression }
    property StatusText: string read GetStatusText;
    property ShowSyntaxErrors: Boolean read GetShowSyntaxErrors write SetShowSyntaxErrors;
    { The lines of the list at the right, after the filter }
    property CompletionCount: Integer read GetCompletionCount;
    property Completions[AIndex: Integer]: TBoldOclCompletion read GetCompletion;
    property MemberOptions: TBoldOclMemberOptions read GetMemberOptions write SetMemberOptions;
    property VariableCount: Integer read GetVariableCount;
    property Variables[AIndex: Integer]: TBoldOclCompletion read GetVariable;
  end;

{ Opens the dialog on AExpression. True when the user closed it with OK;
  AExpression is then the edited expression, otherwise it is left as it was. }
function BoldEditOclExpressionFMX(var AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList = nil): Boolean;

implementation

{ The icons of the lists, PNG files as RCDATA: the resource of the Lazarus
  dialog, which shows the same icons }
{$R '..\..\..\Common\IDELazarus\boldoclexpressionpropedit.res'}

uses
  System.SysUtils,
  System.Types,
  System.Rtti,
  FMX.Platform,
  FMX.DialogService,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF}
  BoldCoreConsts;

const
  cIconResources: array[TBoldOclCompletionKind] of string =
    ('BOLDOCLATTRIBUTE', 'BOLDOCLSINGLELINK', 'BOLDOCLMULTILINK', 'BOLDOCLCLASS', 'BOLDOCLOPERATION');
  cGap = 6;
  cListFontSize = 12;
  // The pointer under an error needs columns
  {$IF Defined(MSWINDOWS)}
  cMonospaceFont = 'Consolas';
  {$ELSEIF Defined(MACOS)}
  cMonospaceFont = 'Menlo';
  {$ELSE}
  cMonospaceFont = 'Monospace';
  {$ENDIF}

{ The captions are shared with the Lazarus dialog, which shows the '&' of an
  accelerator key as an underline; FMX shows it as it is on some platforms }
function NoAccelerator(const ACaption: string): string;
begin
  Result := StringReplace(ACaption, '&', '', [rfReplaceAll]);
end;

procedure OpenUrl(const AUrl: string);
begin
  {$IF Defined(MSWINDOWS)}
  ShellExecute(0, 'open', PChar(AUrl), nil, nil, SW_SHOWNORMAL);
  {$ELSEIF Defined(MACOS)}
  _system(PAnsiChar(UTF8String('open "' + AUrl + '"')));
  {$ELSEIF Defined(POSIX)}
  _system(PAnsiChar(UTF8String('xdg-open "' + AUrl + '" &')));
  {$ENDIF}
end;

function BoldEditOclExpressionFMX(var AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList): Boolean;
var
  Dialog: TBoldOclExpressionDialogFMX;
begin
  Dialog := TBoldOclExpressionDialogFMX.CreateDialog(nil, AContext, AVariables);
  try
    Dialog.Expression := AExpression;
    Result := Dialog.ShowModal = mrOk;
    if Result then
      AExpression := Dialog.Expression;
  finally
    Dialog.Free;
  end;
end;

{ TBoldOclExpressionDialogFMX }

constructor TBoldOclExpressionDialogFMX.CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList; AEvaluator: TBoldEvaluator);
begin
  inherited CreateNew(AOwner);
  fState := TBoldOclEditorState.Create(AContext, AVariables, AEvaluator);
  if Assigned(AContext) then
    Caption := Format(sOclEditorCaption, [AContext.AsString])
  else
    Caption := Format(sOclEditorCaption, [sOclEditorUnknownContext]);
  Position := TFormPosition.ScreenCenter;
  LoadIcons;
  // Setting up a check box changes it
  fUpdating := True;
  try
    BuildControls;
  finally
    fUpdating := False;
  end;
  fState.OnChange := ShowState;
  ShowState(fState);
end;

destructor TBoldOclExpressionDialogFMX.Destroy;
var
  Kind: TBoldOclCompletionKind;
begin
  // The controls' events can come while the form destroys them; they read
  // fState, which goes after them
  fUpdating := True;
  inherited Destroy;
  FreeAndNil(fState);
  for Kind := Low(Kind) to High(Kind) do
    FreeAndNil(fIcons[Kind]);
end;

procedure TBoldOclExpressionDialogFMX.LoadIcons;
var
  Kind: TBoldOclCompletionKind;
  Stream: TResourceStream;
begin
  for Kind := Low(Kind) to High(Kind) do
    try
      Stream := TResourceStream.Create(HInstance, cIconResources[Kind], RT_RCDATA);
      try
        fIcons[Kind] := FMX.Graphics.TBitmap.Create;
        fIcons[Kind].LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    except
      // The lists are shown without the icon; IconCount tells
      on E: Exception do
        FreeAndNil(fIcons[Kind]);
    end;
end;

function TBoldOclExpressionDialogFMX.IconCount: Integer;
var
  Kind: TBoldOclCompletionKind;
begin
  Result := 0;
  for Kind := Low(Kind) to High(Kind) do
    if Assigned(fIcons[Kind]) and (fIcons[Kind].Width = 16) then
      Inc(Result);
end;

procedure TBoldOclExpressionDialogFMX.BuildControls;
var
  BottomBar, RightPanel, MiddlePanel, MessagePanel, ButtonRow, OptionRow, Holder: TLayout;
  Splitter: TSplitter;
  Button: TButton;
  MessagesMenu: TPopupMenu;
  MenuItem: TMenuItem;
  ParserLabel: TLabel;

  function NewLayout(AParent: TFmxObject; AAlign: TAlignLayout): TLayout;
  begin
    Result := TLayout.Create(Self);
    Result.Parent := AParent;
    Result.Align := AAlign;
  end;

  { A row of AButtonCount buttons at the right of AParent. FMX lines up
    right-aligned controls by their right edge, which depends on the width
    the parent has when they are made; left-aligned ones line up from 0, so
    the buttons are left-aligned in a right-aligned holder }
  function NewButtonHolder(AParent: TFmxObject; AButtonCount: Integer): TLayout;
  begin
    Result := NewLayout(AParent, TAlignLayout.Right);
    Result.Width := AButtonCount * (100 + cGap);
  end;

  { The AIndex-th button from the left of its row }
  function NewButton(AParent: TFmxObject; const ACaption: string; AIndex: Integer;
    AOnClick: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Parent := AParent;
    Result.Text := NoAccelerator(ACaption);
    Result.Width := 100;
    Result.Position.X := AIndex * 1000;
    Result.Align := TAlignLayout.Left;
    Result.Margins.Left := cGap;
    Result.OnClick := AOnClick;
  end;

  function NewCheckBox(AParent: TFmxObject; const ACaption: string; APosition: Single): TCheckBox;
  begin
    Result := TCheckBox.Create(Self);
    Result.Parent := AParent;
    Result.Text := NoAccelerator(ACaption);
    Result.Width := 100;
    Result.Position.X := APosition;
    Result.Align := TAlignLayout.Left;
    Result.IsChecked := True;
    Result.OnChange := OptionChanged;
  end;

  function NewList(AParent: TFmxObject): TListBox;
  begin
    Result := TListBox.Create(Self);
    Result.Parent := AParent;
    Result.Align := TAlignLayout.Client;
    Result.ItemHeight := 22;
    Result.OnDblClick := ListDblClick;
    Result.OnKeyDown := ListKeyDown;
  end;

  { The title of a part of the dialog, at its top }
  procedure NewTitle(AParent: TFmxObject; const ACaption: string);
  var
    Title: TLabel;
  begin
    Title := TLabel.Create(Self);
    Title.Parent := AParent;
    Title.Text := NoAccelerator(ACaption);
    Title.Align := TAlignLayout.Top;
    Title.Height := 22;
  end;

begin
  Width := 940;
  Height := 520;
  Constraints.MinWidth := 525;
  Constraints.MinHeight := 300;

  // Bottom: model errors at the left; Show Info, OK and Cancel at the right
  BottomBar := NewLayout(Self, TAlignLayout.Bottom);
  BottomBar.Height := 40;
  BottomBar.Padding.Rect := TRectF.Create(cGap, cGap, cGap, cGap);
  Holder := NewButtonHolder(BottomBar, 3);
  NewButton(Holder, sOclEditorShowInfo, 0, ShowInfoClicked);
  Button := NewButton(Holder, sOclEditorOK, 1, nil);
  Button.ModalResult := mrOk;
  Button := NewButton(Holder, sOclEditorCancel, 2, nil);
  Button.ModalResult := mrCancel;
  Button.Cancel := True;
  fModelErrorsButton := NewButton(BottomBar, sOclEditorModelErrors, 0, ModelErrorsClicked);
  fModelErrorsButton.Width := 120;
  fModelErrorsButton.Margins.Left := 0;

  // Left: the OCL variables
  fVariablePanel := NewLayout(Self, TAlignLayout.Left);
  fVariablePanel.Width := 200;
  fVariablePanel.Position.X := 0;
  fVariablePanel.Margins.Rect := TRectF.Create(cGap, cGap, 0, 0);
  NewTitle(fVariablePanel, sOclEditorVariables);
  fVariableList := NewList(fVariablePanel);
  fVariableSplitter := TSplitter.Create(Self);
  fVariableSplitter.Parent := Self;
  fVariableSplitter.Position.X := 1000;
  fVariableSplitter.Align := TAlignLayout.Left;
  fVariableSplitter.Width := cGap;

  // Right: what can follow the expression, and the splitter left of it
  RightPanel := NewLayout(Self, TAlignLayout.Right);
  RightPanel.Width := 320;
  RightPanel.Margins.Rect := TRectF.Create(0, cGap, cGap, 0);
  NewTitle(RightPanel, sOclEditorAvailableExpressions);
  fFilterCombo := TComboEdit.Create(Self);
  fFilterCombo.Parent := RightPanel;
  fFilterCombo.Position.Y := 1000;
  fFilterCombo.Align := TAlignLayout.Top;
  fFilterCombo.Items.Add(sOclEditorFilterAll);
  fFilterCombo.Items.Add(sOclEditorFilterAttributes);
  fFilterCombo.Items.Add(sOclEditorFilterSingleLinks);
  fFilterCombo.Items.Add(sOclEditorFilterMultiLinks);
  fFilterCombo.Items.Add(sOclEditorFilterOperations);
  fFilterCombo.ItemIndex := 0;
  fFilterCombo.OnChange := FilterChanged;
  fFilterCombo.OnChangeTracking := FilterChanged;
  fFilterCombo.OnExit := FilterExit;
  OptionRow := NewLayout(RightPanel, TAlignLayout.Bottom);
  OptionRow.Height := 28;
  // In the order of Delphi's editor: Derived, Persistent, Transient
  fDerivedCheckBox := NewCheckBox(OptionRow, sOclEditorDerived, 0);
  fPersistentCheckBox := NewCheckBox(OptionRow, sOclEditorPersistent, 100);
  fTransientCheckBox := NewCheckBox(OptionRow, sOclEditorTransient, 200);
  fCompletionList := NewList(RightPanel);
  fCompletionList.Margins.Top := 2;
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Position.X := 0;
  Splitter.Align := TAlignLayout.Right;
  Splitter.Width := cGap;

  // Middle: the expression, and the parser messages under it
  MiddlePanel := NewLayout(Self, TAlignLayout.Client);
  MiddlePanel.Margins.Top := cGap;
  NewTitle(MiddlePanel, sOclEditorExpression);
  // The messages at the bottom, and the splitter above them: bottom-aligned
  // controls line up by their bottom edge, as the right-aligned buttons do
  MessagePanel := NewLayout(MiddlePanel, TAlignLayout.Bottom);
  MessagePanel.Height := 190;
  ButtonRow := NewLayout(MessagePanel, TAlignLayout.Top);
  ButtonRow.Height := 30;
  ButtonRow.Padding.Bottom := 2;
  ParserLabel := TLabel.Create(Self);
  ParserLabel.Parent := ButtonRow;
  ParserLabel.Text := sOclEditorParserMessage;
  ParserLabel.Align := TAlignLayout.Left;
  ParserLabel.Width := 120;
  Holder := NewButtonHolder(ButtonRow, 2);
  NewButton(Holder, sOclEditorRemoveLast, 0, RemoveLastClicked);
  NewButton(Holder, sOclEditorClear, 1, ClearClicked);
  fSyntaxCheckBox := TCheckBox.Create(Self);
  fSyntaxCheckBox.Parent := MessagePanel;
  fSyntaxCheckBox.Text := NoAccelerator(sOclEditorShowOriginalParserErrors);
  fSyntaxCheckBox.Position.Y := 1000;
  fSyntaxCheckBox.Align := TAlignLayout.Top;
  fSyntaxCheckBox.OnChange := OptionChanged;
  fMessagesMemo := TMemo.Create(Self);
  fMessagesMemo.Parent := MessagePanel;
  fMessagesMemo.Align := TAlignLayout.Client;
  fMessagesMemo.ReadOnly := True;
  fMessagesMemo.TabStop := False;
  fMessagesMemo.TextSettings.WordWrap := False;
  fMessagesMemo.StyledSettings := fMessagesMemo.StyledSettings - [TStyledSetting.Family];
  fMessagesMemo.TextSettings.Font.Family := cMonospaceFont;
  MessagesMenu := TPopupMenu.Create(Self);
  MessagesMenu.Parent := Self;
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Text := sOclEditorCopyMessages;
  MenuItem.OnClick := CopyMessagesClicked;
  MenuItem.Parent := MessagesMenu;
  fMessagesMemo.PopupMenu := MessagesMenu;
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := MiddlePanel;
  Splitter.Position.Y := 0;
  Splitter.Align := TAlignLayout.Bottom;
  Splitter.Height := cGap;
  fExpressionMemo := TMemo.Create(Self);
  fExpressionMemo.Parent := MiddlePanel;
  fExpressionMemo.Align := TAlignLayout.Client;
  fExpressionMemo.TextSettings.WordWrap := True;
  fExpressionMemo.OnChangeTracking := ExpressionChanged;
  fExpressionMemo.OnKeyUp := ExpressionKeyUp;
  fExpressionMemo.OnMouseUp := ExpressionMouseUp;
  ActiveControl := fExpressionMemo;
end;

function TBoldOclExpressionDialogFMX.GetExpression: string;
begin
  Result := fState.Expression;
end;

procedure TBoldOclExpressionDialogFMX.SetExpression(const AValue: string);
begin
  // fState shows itself in the memo through ShowState
  fState.Expression := AValue;
end;

function TBoldOclExpressionDialogFMX.GetLastCheck: TBoldOclCheckResult;
begin
  Result := fState.LastCheck;
end;

function TBoldOclExpressionDialogFMX.GetStatusText: string;
begin
  Result := fState.StatusText;
end;

function TBoldOclExpressionDialogFMX.GetCompletionCount: Integer;
begin
  Result := fState.CompletionCount;
end;

function TBoldOclExpressionDialogFMX.GetCompletion(AIndex: Integer): TBoldOclCompletion;
begin
  Result := fState.Completions[AIndex];
end;

function TBoldOclExpressionDialogFMX.GetVariableCount: Integer;
begin
  Result := fState.VariableCount;
end;

function TBoldOclExpressionDialogFMX.GetVariable(AIndex: Integer): TBoldOclCompletion;
begin
  Result := fState.Variables[AIndex];
end;

function TBoldOclExpressionDialogFMX.GetMemberOptions: TBoldOclMemberOptions;
begin
  Result := fState.MemberOptions;
end;

procedure TBoldOclExpressionDialogFMX.SetMemberOptions(AValue: TBoldOclMemberOptions);
begin
  fState.MemberOptions := AValue;
end;

function TBoldOclExpressionDialogFMX.GetShowSyntaxErrors: Boolean;
begin
  Result := fState.ShowSyntaxErrors;
end;

procedure TBoldOclExpressionDialogFMX.SetShowSyntaxErrors(AValue: Boolean);
begin
  fState.ShowSyntaxErrors := AValue;
end;

{ The OnChange of fState: the controls show it again }
procedure TBoldOclExpressionDialogFMX.ShowState(Sender: TObject);
var
  WasUpdating: Boolean;
begin
  WasUpdating := fUpdating;
  fUpdating := True;
  try
    // Not while the user types, which would move the caret
    if BoldOclTrimLineBreaks(fExpressionMemo.Text) <> fState.Expression then
    begin
      fExpressionMemo.Text := fState.Expression;
      fExpressionMemo.SelStart := Length(fState.Expression);
    end;
    fMessagesMemo.Text := fState.StatusText;
    fSyntaxCheckBox.IsChecked := fState.ShowSyntaxErrors;
    fDerivedCheckBox.IsChecked := omoDerived in fState.MemberOptions;
    fPersistentCheckBox.IsChecked := omoPersistent in fState.MemberOptions;
    fTransientCheckBox.IsChecked := omoTransient in fState.MemberOptions;
    if (fState.KindFilter >= 0) and (fState.KindFilter < fFilterCombo.Items.Count) then
    begin
      // Also after a typed filter, which can leave the old line selected
      if (fFilterCombo.ItemIndex <> fState.KindFilter) or
        (fFilterCombo.Text <> fFilterCombo.Items[fState.KindFilter]) then
      begin
        fFilterCombo.Text := fFilterCombo.Items[fState.KindFilter];
        fFilterCombo.ItemIndex := fState.KindFilter;
      end;
    end
    else if (fState.KindFilter = ocfText) and (fFilterCombo.Text <> fState.FilterText) then
    begin
      fFilterCombo.ItemIndex := -1;
      fFilterCombo.Text := fState.FilterText;
    end;
    FillList(fCompletionList, False);
    FillList(fVariableList, True);
    fVariablePanel.Visible := fState.VariableCount > 0;
    fVariableSplitter.Visible := fVariablePanel.Visible;
    fModelErrorsButton.Visible := fState.ModelErrors <> '';
  finally
    fUpdating := WasUpdating;
  end;
end;

procedure TBoldOclExpressionDialogFMX.FillList(AList: TListBox; AVariables: Boolean);
var
  i, Count: Integer;
  Item: TBoldOclCompletion;
  ListItem: TListBoxItem;
begin
  if AVariables then
    Count := fState.VariableCount
  else
    Count := fState.CompletionCount;
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to Count - 1 do
    begin
      if AVariables then
        Item := fState.Variables[i]
      else
        Item := fState.Completions[i];
      ListItem := TListBoxItem.Create(AList);
      ListItem.Parent := AList;
      // The default style of a line has no place for ItemData.Bitmap; this
      // one has, with a larger font than the rest of the dialog
      ListItem.StyleLookup := 'listboxitemnodetail';
      ListItem.StyledSettings := ListItem.StyledSettings - [TStyledSetting.Size];
      ListItem.TextSettings.Font.Size := cListFontSize;
      ListItem.Text := Item.Name + Item.Detail;
      if Assigned(fIcons[Item.Kind]) then
        ListItem.ItemData.Bitmap := fIcons[Item.Kind];
    end;
  finally
    AList.EndUpdate;
  end;
end;

procedure TBoldOclExpressionDialogFMX.SetFilter(AKindFilter: Integer; const AText: string);
begin
  fState.SetFilter(AKindFilter, AText);
end;

procedure TBoldOclExpressionDialogFMX.AddCompletion(AIndex: Integer);
begin
  fState.AddCompletion(AIndex);
end;

procedure TBoldOclExpressionDialogFMX.AddVariable(AIndex: Integer);
begin
  fState.AddVariable(AIndex);
end;

procedure TBoldOclExpressionDialogFMX.RemoveLast;
begin
  fState.RemoveLast;
end;

procedure TBoldOclExpressionDialogFMX.ClearExpression;
begin
  fState.Clear;
end;

procedure TBoldOclExpressionDialogFMX.SelectPlaceholder;
var
  Start, Count: Integer;
begin
  if (fExpressionMemo.SelLength = 0) and
    BoldOclPlaceholderAt(fExpressionMemo.Text, fExpressionMemo.SelStart, Start, Count) then
  begin
    fExpressionMemo.SelStart := Start;
    fExpressionMemo.SelLength := Count;
  end;
end;

procedure TBoldOclExpressionDialogFMX.ExpressionChanged(Sender: TObject);
begin
  if not fUpdating then
    fState.Expression := fExpressionMemo.Text;
end;

procedure TBoldOclExpressionDialogFMX.ExpressionKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  SelectPlaceholder;
end;

procedure TBoldOclExpressionDialogFMX.ExpressionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SelectPlaceholder;
end;

procedure TBoldOclExpressionDialogFMX.OptionChanged(Sender: TObject);
var
  Options: TBoldOclMemberOptions;
  OldIndex: Integer;
begin
  if fUpdating then
    Exit;
  if Sender = fSyntaxCheckBox then
  begin
    fState.ShowSyntaxErrors := fSyntaxCheckBox.IsChecked;
    Exit;
  end;
  Options := [];
  if fDerivedCheckBox.IsChecked then
    Include(Options, omoDerived);
  if fPersistentCheckBox.IsChecked then
    Include(Options, omoPersistent);
  if fTransientCheckBox.IsChecked then
    Include(Options, omoTransient);
  // The list keeps its selected line
  OldIndex := fCompletionList.ItemIndex;
  fState.MemberOptions := Options;
  if OldIndex < fCompletionList.Count then
    fCompletionList.ItemIndex := OldIndex;
end;

procedure TBoldOclExpressionDialogFMX.FilterChanged(Sender: TObject);
begin
  if fUpdating then
    Exit;
  // A line of the filter's list, or a text typed there
  if (fFilterCombo.ItemIndex < 0) or (fFilterCombo.ItemIndex >= fFilterCombo.Items.Count) or
    (fFilterCombo.Items[fFilterCombo.ItemIndex] <> fFilterCombo.Text) then
    fState.SetFilter(ocfText, fFilterCombo.Text)
  else
    fState.SetFilter(fFilterCombo.ItemIndex);
end;

procedure TBoldOclExpressionDialogFMX.FilterExit(Sender: TObject);
begin
  if not fUpdating and (fState.KindFilter = ocfText) and (Trim(fFilterCombo.Text) = '') then
    fState.SetFilter(ocfAll);
end;

procedure TBoldOclExpressionDialogFMX.ListDblClick(Sender: TObject);
var
  List: TListBox;
begin
  List := Sender as TListBox;
  if List.Count = 0 then
    Exit;
  if List.ItemIndex = -1 then
    List.ItemIndex := 0;
  if List = fVariableList then
    AddVariable(List.ItemIndex)
  else
    AddCompletion(List.ItemIndex);
end;

procedure TBoldOclExpressionDialogFMX.ListKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    ListDblClick(Sender);
    Key := 0;
  end;
end;

procedure TBoldOclExpressionDialogFMX.ClearClicked(Sender: TObject);
begin
  ClearExpression;
end;

procedure TBoldOclExpressionDialogFMX.RemoveLastClicked(Sender: TObject);
begin
  RemoveLast;
end;

procedure TBoldOclExpressionDialogFMX.ShowInfoClicked(Sender: TObject);
begin
  OpenUrl(cBoldOclInfoUrl);
end;

procedure TBoldOclExpressionDialogFMX.ModelErrorsClicked(Sender: TObject);
begin
  TDialogService.ShowMessage(sModelContainsErrors + sLineBreak + sLineBreak + fState.ModelErrors);
end;

procedure TBoldOclExpressionDialogFMX.CopyMessagesClicked(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
    Clipboard.SetClipboard(fState.MessagesAsText);
end;

end.
