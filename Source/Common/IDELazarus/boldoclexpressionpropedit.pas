
{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionPropEdit;

{ The Lazarus property editor of an OCL expression (#117): the "..." button
  of Expression in the Object Inspector opens Delphi's OCL editor
  (BoldOclPropEditor) as an LCL dialog. In the middle the expression and,
  under it, the parser messages: the type of the result, or the expression
  with a pointer at the error. At the right what can follow the expression:
  the members of its type (or the classes, for the system) and the
  operations, with a filter; a double click or Enter adds the line to the
  expression. At the left the OCL variables. The dialog holds a
  TBoldOclEditorState (BoldOclExpressionCheck), as the FMX dialog
  BoldOclExpressionDialogFMX does: that checks, makes the lists and applies
  the filter, and the dialog shows it.

  Not here from the Delphi editor: the font button and the list of recently
  used expressions, which are kept in the Windows registry. }

interface

uses
  Classes,
  Types,
  Controls,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
  Menus,
  PropEdits,
  BoldElements,
  BoldOclExpressionCheck;

type
  { TBoldOclExpressionDialog }
  TBoldOclExpressionDialog = class(TForm)
  private
    fState: TBoldOclEditorState;
    fExpressionMemo: TMemo;
    fMessagesMemo: TMemo;
    fSyntaxCheckBox: TCheckBox;
    fFilterCombo: TComboBox;
    fCompletionList: TListBox;
    fVariableList: TListBox;
    fVariablePanel: TGroupBox;
    fDerivedCheckBox, fPersistentCheckBox, fTransientCheckBox: TCheckBox;
    fModelErrorsButton: TButton;
    fIcons: array[TBoldOclCompletionKind] of TPortableNetworkGraphic;
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
    { The memo's text, without the line break a memo ends it with }
    function MemoExpression: string;
    procedure LoadIcons;
    procedure BuildControls;
    procedure ShowState(Sender: TObject);
    procedure ShowCompletions;
    procedure SelectPlaceholder;
    procedure ExpressionChanged(Sender: TObject);
    procedure ExpressionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExpressionMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OptionClicked(Sender: TObject);
    procedure FilterChanged(Sender: TObject);
    procedure FilterExit(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyPress(Sender: TObject; var Key: AnsiChar);
    procedure ListDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure ClearClicked(Sender: TObject);
    procedure RemoveLastClicked(Sender: TObject);
    procedure ShowInfoClicked(Sender: TObject);
    procedure ModelErrorsClicked(Sender: TObject);
    procedure CopyMessagesClicked(Sender: TObject);
  public
    { AContext may be nil: the expression can be edited but is not checked }
    constructor CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
      AVariables: TBoldExternalVariableList);
    destructor Destroy; override;
    { The filter of the list at the right: a line of its list (ocfAll ..
      ocfOperations), or with ocfText a text the lines must contain }
    procedure SetFilter(AKindFilter: Integer; const AText: string = '');
    { What a double click on the line does: adds it to the expression }
    procedure AddCompletion(AIndex: Integer);
    procedure AddVariable(AIndex: Integer);
    procedure RemoveLast;
    procedure ClearExpression;
    { The icons of the lists, one for each kind of line }
    function IconCount: Integer;
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

  { TBoldOclExpressionPropertyEditor }
  { TBoldExpression is string, a UnicodeString in Bold's mode delphiunicode }
  TBoldOclExpressionPropertyEditor = class(TUnicodeStringPropertyEditor)
  protected
    { True when the user closed the dialog with OK }
    function ShowDialog(ADialog: TBoldOclExpressionDialog): Boolean; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ The type an expression of APersistent is evaluated in, and its variables:
  a handle with an expression (IBoldOCLComponent) or a follower controller,
  as the BoldProperties of a TBoldDataSet field. False when APersistent is
  neither; AContext is nil when the component is not connected to a model. }
function BoldOclContextOf(APersistent: TPersistent; out AContext: TBoldElementTypeInfo;
  out AVariables: TBoldExternalVariableList): Boolean;

implementation

{$R boldoclexpressionpropedit.res}

uses
  SysUtils,
  TypInfo,
  LCLType,
  LCLIntf,
  Clipbrd,
  Dialogs,
  BoldCoreConsts,
  BoldControlPack;

{ The captions of the dialog are in BoldCoreConsts, shared with the FMX dialog }
resourcestring
  sNotAnExpressionOwner = '%s has no OCL expression to edit';

const
  cIconResources: array[TBoldOclCompletionKind] of string =
    ('BOLDOCLATTRIBUTE', 'BOLDOCLSINGLELINK', 'BOLDOCLMULTILINK', 'BOLDOCLCLASS', 'BOLDOCLOPERATION');
  cGap = 6;

function BoldOclContextOf(APersistent: TPersistent; out AContext: TBoldElementTypeInfo;
  out AVariables: TBoldExternalVariableList): Boolean;
var
  OclComponent: IBoldOCLComponent;
begin
  AContext := nil;
  AVariables := nil;
  Result := True;
  if APersistent is TBoldFollowerController then
  begin
    AContext := TBoldFollowerController(APersistent).ContextType;
    AVariables := TBoldFollowerController(APersistent).VariableList;
  end
  else if Assigned(APersistent) and APersistent.GetInterface(IBoldOCLComponent, OclComponent) then
  begin
    AContext := OclComponent.ContextType;
    AVariables := OclComponent.VariableList;
  end
  else
    Result := False;
end;

{ TBoldOclExpressionDialog }

constructor TBoldOclExpressionDialog.CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList);
begin
  inherited CreateNew(AOwner);
  fState := TBoldOclEditorState.Create(AContext, AVariables);
  if Assigned(AContext) then
    Caption := Format(sOclEditorCaption, [AContext.AsString])
  else
    Caption := Format(sOclEditorCaption, [sOclEditorUnknownContext]);
  Position := poScreenCenter;
  LoadIcons;
  // Setting up a check box clicks it
  fUpdating := True;
  try
    BuildControls;
  finally
    fUpdating := False;
  end;
  fState.OnChange := ShowState;
  ShowState(fState);
end;

destructor TBoldOclExpressionDialog.Destroy;
var
  Kind: TBoldOclCompletionKind;
begin
  // The controls' events can come while the form destroys them; they read
  // fState and the icons, which go after them
  fUpdating := True;
  inherited Destroy;
  FreeAndNil(fState);
  for Kind := Low(Kind) to High(Kind) do
    FreeAndNil(fIcons[Kind]);
end;

procedure TBoldOclExpressionDialog.LoadIcons;
var
  Kind: TBoldOclCompletionKind;
  Stream: TResourceStream;
begin
  for Kind := Low(Kind) to High(Kind) do
    try
      Stream := TResourceStream.Create(HInstance, cIconResources[Kind], RT_RCDATA);
      try
        fIcons[Kind] := TPortableNetworkGraphic.Create;
        fIcons[Kind].LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    except
      // The list is drawn without the icon
      on E: Exception do
        FreeAndNil(fIcons[Kind]);
    end;
end;

function TBoldOclExpressionDialog.IconCount: Integer;
var
  Kind: TBoldOclCompletionKind;
begin
  Result := 0;
  for Kind := Low(Kind) to High(Kind) do
    if Assigned(fIcons[Kind]) and (fIcons[Kind].Width = 16) then
      Inc(Result);
end;

procedure TBoldOclExpressionDialog.BuildControls;
var
  BottomPanel, RightPanel, MiddlePanel, MessagePanel, ButtonRow, OptionRow: TPanel;
  RightBox, ExpressionBox: TGroupBox;
  Splitter: TSplitter;
  Button: TButton;
  MessagesMenu: TPopupMenu;
  MenuItem: TMenuItem;

  function NewPanel(AParent: TWinControl; AAlign: TAlign): TPanel;
  begin
    Result := TPanel.Create(Self);
    Result.Parent := AParent;
    Result.Align := AAlign;
    Result.BevelOuter := bvNone;
    Result.Caption := '';
  end;

  function NewButton(AParent: TWinControl; const ACaption: string; AAlign: TAlign; AOnClick: TNotifyEvent): TButton;
  begin
    Result := TButton.Create(Self);
    Result.Parent := AParent;
    Result.Caption := ACaption;
    Result.Align := AAlign;
    Result.AutoSize := True;
    Result.Constraints.MinWidth := 81;
    Result.BorderSpacing.Left := cGap;
    Result.OnClick := AOnClick;
  end;

  function NewCheckBox(AParent: TWinControl; const ACaption: string; AOnClick: TNotifyEvent): TCheckBox;
  begin
    Result := TCheckBox.Create(Self);
    Result.Parent := AParent;
    Result.Caption := ACaption;
    Result.Align := alLeft;
    Result.BorderSpacing.Right := cGap;
    Result.Checked := True;
    Result.OnClick := AOnClick;
  end;

  function NewList(AParent: TWinControl): TListBox;
  begin
    Result := TListBox.Create(Self);
    Result.Parent := AParent;
    Result.Align := alClient;
    Result.Style := lbOwnerDrawFixed;
    Result.ItemHeight := 22;
    Result.OnDblClick := ListDblClick;
    Result.OnKeyPress := ListKeyPress;
    Result.OnDrawItem := ListDrawItem;
  end;

begin
  Width := 940;
  Height := 480;
  Constraints.MinWidth := 525;
  Constraints.MinHeight := 264;

  // Bottom: model errors at the left; Show Info, OK and Cancel at the right
  BottomPanel := NewPanel(Self, alBottom);
  BottomPanel.AutoSize := True;
  BottomPanel.BorderSpacing.Around := cGap;
  // Controls with alRight line up from the right in the order they are made
  Button := NewButton(BottomPanel, sOclEditorCancel, alRight, nil);
  Button.ModalResult := mrCancel;
  Button.Cancel := True;
  Button.Left := 3000;
  Button := NewButton(BottomPanel, sOclEditorOK, alRight, nil);
  Button.ModalResult := mrOK;
  Button.Left := 2000;
  Button := NewButton(BottomPanel, sOclEditorShowInfo, alRight, ShowInfoClicked);
  Button.Left := 1000;
  fModelErrorsButton := NewButton(BottomPanel, sOclEditorModelErrors, alLeft, ModelErrorsClicked);
  fModelErrorsButton.BorderSpacing.Left := 0;

  // Left: the OCL variables
  fVariablePanel := TGroupBox.Create(Self);
  fVariablePanel.Parent := Self;
  fVariablePanel.Caption := sOclEditorVariables;
  fVariablePanel.Align := alLeft;
  fVariablePanel.Width := 200;
  fVariablePanel.BorderSpacing.Left := cGap;
  fVariablePanel.BorderSpacing.Top := cGap;
  fVariableList := NewList(fVariablePanel);
  fVariableList.BorderSpacing.Around := 2;
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alLeft;
  Splitter.Left := fVariablePanel.Width + 1;

  // Right: what can follow the expression
  RightBox := TGroupBox.Create(Self);
  RightBox.Parent := Self;
  RightBox.Caption := sOclEditorAvailableExpressions;
  RightBox.Align := alRight;
  RightBox.Width := 320;
  RightBox.BorderSpacing.Right := cGap;
  RightBox.BorderSpacing.Top := cGap;
  RightPanel := NewPanel(RightBox, alClient);
  RightPanel.BorderSpacing.Around := 2;
  fFilterCombo := TComboBox.Create(Self);
  fFilterCombo.Parent := RightPanel;
  fFilterCombo.Align := alTop;
  fFilterCombo.Items.Add(sOclEditorFilterAll);
  fFilterCombo.Items.Add(sOclEditorFilterAttributes);
  fFilterCombo.Items.Add(sOclEditorFilterSingleLinks);
  fFilterCombo.Items.Add(sOclEditorFilterMultiLinks);
  fFilterCombo.Items.Add(sOclEditorFilterOperations);
  fFilterCombo.ItemIndex := 0;
  fFilterCombo.OnChange := FilterChanged;
  fFilterCombo.OnExit := FilterExit;
  OptionRow := NewPanel(RightPanel, alBottom);
  OptionRow.AutoSize := True;
  fDerivedCheckBox := NewCheckBox(OptionRow, sOclEditorDerived, OptionClicked);
  fPersistentCheckBox := NewCheckBox(OptionRow, sOclEditorPersistent, OptionClicked);
  fTransientCheckBox := NewCheckBox(OptionRow, sOclEditorTransient, OptionClicked);
  // In the order of Delphi's editor: Derived, Persistent, Transient
  fDerivedCheckBox.Left := 0;
  fPersistentCheckBox.Left := 100;
  fTransientCheckBox.Left := 200;
  fCompletionList := NewList(RightPanel);
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := Self;
  Splitter.Align := alRight;
  Splitter.Left := RightBox.Left - 1;

  // Middle: the expression, and the parser messages under it
  ExpressionBox := TGroupBox.Create(Self);
  ExpressionBox.Parent := Self;
  ExpressionBox.Caption := sOclEditorExpression;
  ExpressionBox.Align := alClient;
  ExpressionBox.BorderSpacing.Top := cGap;
  MiddlePanel := NewPanel(ExpressionBox, alClient);
  MiddlePanel.BorderSpacing.Around := 2;
  MessagePanel := NewPanel(MiddlePanel, alBottom);
  MessagePanel.Height := 170;
  ButtonRow := NewPanel(MessagePanel, alTop);
  ButtonRow.AutoSize := True;
  ButtonRow.BorderSpacing.Bottom := 2;
  with TLabel.Create(Self) do
  begin
    Parent := ButtonRow;
    Caption := sOclEditorParserMessage;
    Align := alLeft;
    Layout := tlCenter;
    BorderSpacing.Right := cGap;
  end;
  Button := NewButton(ButtonRow, sOclEditorClear, alRight, ClearClicked);
  Button.Left := 2000;
  Button := NewButton(ButtonRow, sOclEditorRemoveLast, alRight, RemoveLastClicked);
  Button.Left := 1000;
  fSyntaxCheckBox := NewCheckBox(MessagePanel, sOclEditorShowOriginalParserErrors, OptionClicked);
  fSyntaxCheckBox.Align := alTop;
  fSyntaxCheckBox.Checked := False;
  fSyntaxCheckBox.Top := ButtonRow.Height + 1;
  fMessagesMemo := TMemo.Create(Self);
  fMessagesMemo.Parent := MessagePanel;
  fMessagesMemo.Align := alClient;
  fMessagesMemo.ReadOnly := True;
  fMessagesMemo.TabStop := False;
  fMessagesMemo.ParentColor := True;
  fMessagesMemo.WordWrap := False;
  fMessagesMemo.ScrollBars := ssAutoBoth;
  // The pointer under an error needs columns
  fMessagesMemo.Font.Name := 'Monospace';
  fMessagesMemo.Font.Pitch := fpFixed;
  MessagesMenu := TPopupMenu.Create(Self);
  MenuItem := TMenuItem.Create(Self);
  MenuItem.Caption := sOclEditorCopyMessages;
  MenuItem.OnClick := CopyMessagesClicked;
  MessagesMenu.Items.Add(MenuItem);
  fMessagesMemo.PopupMenu := MessagesMenu;
  Splitter := TSplitter.Create(Self);
  Splitter.Parent := MiddlePanel;
  Splitter.Align := alBottom;
  Splitter.Top := MessagePanel.Top - 1;
  fExpressionMemo := TMemo.Create(Self);
  fExpressionMemo.Parent := MiddlePanel;
  fExpressionMemo.Align := alClient;
  fExpressionMemo.Constraints.MinHeight := 50;
  fExpressionMemo.WordWrap := True;
  fExpressionMemo.ScrollBars := ssAutoVertical;
  fExpressionMemo.OnChange := ExpressionChanged;
  fExpressionMemo.OnKeyUp := ExpressionKeyUp;
  fExpressionMemo.OnMouseUp := ExpressionMouseUp;
  ActiveControl := fExpressionMemo;
end;

function TBoldOclExpressionDialog.MemoExpression: string;
begin
  Result := BoldOclTrimLineBreaks(fExpressionMemo.Lines.Text);
end;

function TBoldOclExpressionDialog.GetExpression: string;
begin
  Result := fState.Expression;
end;

procedure TBoldOclExpressionDialog.SetExpression(const AValue: string);
begin
  // fState shows itself in the memo through ShowState; not left to the
  // memo's OnChange, which a memo without a window handle does not fire
  fState.Expression := AValue;
end;

function TBoldOclExpressionDialog.GetLastCheck: TBoldOclCheckResult;
begin
  Result := fState.LastCheck;
end;

function TBoldOclExpressionDialog.GetStatusText: string;
begin
  Result := fState.StatusText;
end;

function TBoldOclExpressionDialog.GetCompletionCount: Integer;
begin
  Result := fState.CompletionCount;
end;

function TBoldOclExpressionDialog.GetCompletion(AIndex: Integer): TBoldOclCompletion;
begin
  Result := fState.Completions[AIndex];
end;

function TBoldOclExpressionDialog.GetVariableCount: Integer;
begin
  Result := fState.VariableCount;
end;

function TBoldOclExpressionDialog.GetVariable(AIndex: Integer): TBoldOclCompletion;
begin
  Result := fState.Variables[AIndex];
end;

function TBoldOclExpressionDialog.GetMemberOptions: TBoldOclMemberOptions;
begin
  Result := fState.MemberOptions;
end;

procedure TBoldOclExpressionDialog.SetMemberOptions(AValue: TBoldOclMemberOptions);
begin
  fState.MemberOptions := AValue;
end;

function TBoldOclExpressionDialog.GetShowSyntaxErrors: Boolean;
begin
  Result := fState.ShowSyntaxErrors;
end;

procedure TBoldOclExpressionDialog.SetShowSyntaxErrors(AValue: Boolean);
begin
  fState.ShowSyntaxErrors := AValue;
end;

{ The OnChange of fState: the controls show it again }
procedure TBoldOclExpressionDialog.ShowState(Sender: TObject);
var
  i: Integer;
  WasUpdating: Boolean;
begin
  WasUpdating := fUpdating;
  fUpdating := True;
  try
    // Not while the user types, which would move the caret
    if MemoExpression <> fState.Expression then
    begin
      fExpressionMemo.Lines.Text := fState.Expression;
      fExpressionMemo.SelStart := Length(fState.Expression);
    end;
    fMessagesMemo.Lines.Text := fState.StatusText;
    fSyntaxCheckBox.Checked := fState.ShowSyntaxErrors;
    fDerivedCheckBox.Checked := omoDerived in fState.MemberOptions;
    fPersistentCheckBox.Checked := omoPersistent in fState.MemberOptions;
    fTransientCheckBox.Checked := omoTransient in fState.MemberOptions;
    if (fState.KindFilter >= 0) and (fState.KindFilter < fFilterCombo.Items.Count) then
    begin
      // Also after a typed filter, which leaves the old line selected
      if (fFilterCombo.ItemIndex <> fState.KindFilter) or
        (fFilterCombo.Text <> fFilterCombo.Items[fState.KindFilter]) then
      begin
        fFilterCombo.ItemIndex := fState.KindFilter;
        fFilterCombo.Text := fFilterCombo.Items[fState.KindFilter];
      end;
    end
    else if (fState.KindFilter = ocfText) and (fFilterCombo.Text <> fState.FilterText) then
    begin
      fFilterCombo.ItemIndex := -1;
      fFilterCombo.Text := fState.FilterText;
    end;
    ShowCompletions;
    fVariableList.Items.BeginUpdate;
    try
      fVariableList.Items.Clear;
      for i := 0 to fState.VariableCount - 1 do
        fVariableList.Items.Add(fState.Variables[i].Name + fState.Variables[i].Detail);
    finally
      fVariableList.Items.EndUpdate;
    end;
    fVariablePanel.Visible := fState.VariableCount > 0;
    fModelErrorsButton.Visible := fState.ModelErrors <> '';
  finally
    fUpdating := WasUpdating;
  end;
end;

procedure TBoldOclExpressionDialog.ShowCompletions;
var
  i: Integer;
begin
  fCompletionList.Items.BeginUpdate;
  try
    fCompletionList.Items.Clear;
    for i := 0 to fState.CompletionCount - 1 do
      fCompletionList.Items.Add(fState.Completions[i].Name + fState.Completions[i].Detail);
  finally
    fCompletionList.Items.EndUpdate;
  end;
end;

procedure TBoldOclExpressionDialog.SetFilter(AKindFilter: Integer; const AText: string);
begin
  fState.SetFilter(AKindFilter, AText);
end;

procedure TBoldOclExpressionDialog.AddCompletion(AIndex: Integer);
begin
  fState.AddCompletion(AIndex);
end;

procedure TBoldOclExpressionDialog.AddVariable(AIndex: Integer);
begin
  fState.AddVariable(AIndex);
end;

procedure TBoldOclExpressionDialog.RemoveLast;
begin
  fState.RemoveLast;
end;

procedure TBoldOclExpressionDialog.ClearExpression;
begin
  fState.Clear;
end;

procedure TBoldOclExpressionDialog.SelectPlaceholder;
var
  Start, Count: Integer;
begin
  if (fExpressionMemo.SelLength = 0) and
    BoldOclPlaceholderAt(fExpressionMemo.Lines.Text, fExpressionMemo.SelStart, Start, Count) then
  begin
    fExpressionMemo.SelStart := Start;
    fExpressionMemo.SelLength := Count;
  end;
end;

procedure TBoldOclExpressionDialog.ExpressionChanged(Sender: TObject);
begin
  if not fUpdating then
    fState.Expression := MemoExpression;
end;

procedure TBoldOclExpressionDialog.ExpressionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SelectPlaceholder;
end;

procedure TBoldOclExpressionDialog.ExpressionMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SelectPlaceholder;
end;

procedure TBoldOclExpressionDialog.OptionClicked(Sender: TObject);
var
  Options: TBoldOclMemberOptions;
  OldIndex, OldTop: Integer;
begin
  if fUpdating then
    Exit;
  if Sender = fSyntaxCheckBox then
  begin
    fState.ShowSyntaxErrors := fSyntaxCheckBox.Checked;
    Exit;
  end;
  Options := [];
  if fDerivedCheckBox.Checked then
    Include(Options, omoDerived);
  if fPersistentCheckBox.Checked then
    Include(Options, omoPersistent);
  if fTransientCheckBox.Checked then
    Include(Options, omoTransient);
  // The list keeps its place
  OldIndex := fCompletionList.ItemIndex;
  OldTop := fCompletionList.TopIndex;
  fState.MemberOptions := Options;
  if OldIndex < fCompletionList.Items.Count then
    fCompletionList.ItemIndex := OldIndex;
  fCompletionList.TopIndex := OldTop;
end;

procedure TBoldOclExpressionDialog.FilterChanged(Sender: TObject);
begin
  if fUpdating then
    Exit;
  // A line of the filter's list, or a text typed there
  if (fFilterCombo.ItemIndex < 0) or (fFilterCombo.Items[fFilterCombo.ItemIndex] <> fFilterCombo.Text) then
    fState.SetFilter(ocfText, fFilterCombo.Text)
  else
    fState.SetFilter(fFilterCombo.ItemIndex);
end;

procedure TBoldOclExpressionDialog.FilterExit(Sender: TObject);
begin
  if not fUpdating and (fFilterCombo.ItemIndex = -1) and (fFilterCombo.Text = '') then
    fState.SetFilter(ocfAll);
end;

procedure TBoldOclExpressionDialog.ListDblClick(Sender: TObject);
var
  List: TListBox;
begin
  List := Sender as TListBox;
  if List.Items.Count = 0 then
    Exit;
  if List.ItemIndex = -1 then
    List.ItemIndex := 0;
  if List = fVariableList then
    AddVariable(List.ItemIndex)
  else
    AddCompletion(List.ItemIndex);
end;

procedure TBoldOclExpressionDialog.ListKeyPress(Sender: TObject; var Key: AnsiChar);
begin
  if Key = #13 then
  begin
    ListDblClick(Sender);
    Key := #0;
  end;
end;

procedure TBoldOclExpressionDialog.ListDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  List: TListBox;
  Item: TBoldOclCompletion;
  X, Y: Integer;
  Name: AnsiString;
begin
  List := Control as TListBox;
  if List = fVariableList then
  begin
    if (Index < 0) or (Index >= fState.VariableCount) then
      Exit;
    Item := fState.Variables[Index];
  end
  else
  begin
    if (Index < 0) or (Index >= fState.CompletionCount) then
      Exit;
    Item := fState.Completions[Index];
  end;
  List.Canvas.FillRect(ARect);
  if Assigned(fIcons[Item.Kind]) then
    List.Canvas.Draw(ARect.Left + 2, ARect.Top + (ARect.Bottom - ARect.Top - 16) div 2, fIcons[Item.Kind]);
  X := ARect.Left + 2 + 16 + 4;
  Name := UTF8Encode(Item.Name);
  Y := ARect.Top + (ARect.Bottom - ARect.Top - List.Canvas.TextHeight(Name)) div 2;
  List.Canvas.Brush.Style := bsClear;
  List.Canvas.TextOut(X, Y, Name);
  // The arguments and the type are grey, except in the selected line
  if not (odSelected in State) then
    List.Canvas.Font.Color := clGrayText;
  List.Canvas.TextOut(X + List.Canvas.TextWidth(Name), Y, UTF8Encode(Item.Detail));
  List.Canvas.Brush.Style := bsSolid;
end;

procedure TBoldOclExpressionDialog.ClearClicked(Sender: TObject);
begin
  ClearExpression;
end;

procedure TBoldOclExpressionDialog.RemoveLastClicked(Sender: TObject);
begin
  RemoveLast;
end;

procedure TBoldOclExpressionDialog.ShowInfoClicked(Sender: TObject);
begin
  OpenURL(cBoldOclInfoUrl);
end;

procedure TBoldOclExpressionDialog.ModelErrorsClicked(Sender: TObject);
begin
  ShowMessage(sModelContainsErrors + LineEnding + LineEnding + fState.ModelErrors);
end;

procedure TBoldOclExpressionDialog.CopyMessagesClicked(Sender: TObject);
begin
  Clipboard.AsText := UTF8Encode(fState.MessagesAsText);
end;

{ TBoldOclExpressionPropertyEditor }

function TBoldOclExpressionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect];
end;

function TBoldOclExpressionPropertyEditor.ShowDialog(ADialog: TBoldOclExpressionDialog): Boolean;
begin
  Result := ADialog.ShowModal = mrOK;
end;

procedure TBoldOclExpressionPropertyEditor.Edit;
var
  Context: TBoldElementTypeInfo;
  Variables: TBoldExternalVariableList;
  Dialog: TBoldOclExpressionDialog;
begin
  if not BoldOclContextOf(GetComponent(0), Context, Variables) then
    raise EPropertyError.CreateFmt(sNotAnExpressionOwner, [GetComponent(0).ClassName]);
  Dialog := TBoldOclExpressionDialog.CreateDialog(nil, Context, Variables);
  try
    Dialog.Expression := GetUnicodeStrValue;
    if ShowDialog(Dialog) and (Dialog.Expression <> GetUnicodeStrValue) then
      SetUnicodeStrValue(Dialog.Expression);
  finally
    Dialog.Free;
  end;
end;

end.
