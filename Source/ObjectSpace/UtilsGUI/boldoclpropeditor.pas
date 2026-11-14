{ Global compiler directives }
{$include bold.inc}
unit BoldOclPropEditor;

interface

uses
  Windows,
  Messages,
  Dialogs,
  Classes,
  Graphics,
  Controls,
  Forms,
  ClipBrd,
  StdCtrls,
  BoldHandles,
  BoldAbstractModel,
  BoldSystemRT,
  BoldElements,
  BoldOcl,
  BoldOclExpressionCheck,
  ExtCtrls,
  Grids,
  ComCtrls,
  ImgList,
  Menus,
  ShellAPI,
  ImageList;

type
  TBoldOclPropEditForm = class(TForm)
    ExpBottomPanel: TPanel;
    TabControlPanel: TPanel;
    ExprEditPageControl: TPageControl;
    ExpressionPage: TTabSheet;
    pnlClient: TPanel;
    ExpParserPanel: TPanel;
    EditPanel: TPanel;
    EditMemo: TMemo;
    pnlEditButtons: TPanel;
    Syntaxcb: TCheckBox;
    Clear: TButton;
    RemoveLast: TButton;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Copymessagestoclipboard1: TMenuItem;
    ParserMessages: TMemo;
    ParserMsg: TLabel;
    Splitter2: TSplitter;
    imgModelErrors: TImage;
    pnlOKCancel: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnShowInfo: TButton;
    VariablesPageControl: TPageControl;
    OclVariablesTabSheet: TTabSheet;
    pnlOclVariables: TPanel;
    VariablesListBox: TListBox;
    ExpressionTypePageControl: TPageControl;
    ExpressionTypeTabSheet: TTabSheet;
    pnlRightSide: TPanel;
    SelectBox: TListBox;
    pnlFilterPersistence: TPanel;
    Derivedcb: TCheckBox;
    SplitterRight: TSplitter;
    SplitterLeft: TSplitter;
    filterCombo: TComboBox;
    MRUPopupMenu: TPopupMenu;
    Persistentcb: TCheckBox;
    Transientcb: TCheckBox;
    btnSetFont: TButton;
    FontDialog1: TFontDialog;
    procedure btnSetFontClick(Sender: TObject);
    procedure SelectBoxDblClick(Sender: TObject);
    procedure EditMemoEnter(Sender: TObject);
    procedure SelectBoxKeyPress(Sender: TObject; var Key: Char);
    procedure EditMemoChange(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure RemoveLastClick(Sender: TObject);
    procedure EditMemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditMemoKeyPress(Sender: TObject; var Key: Char);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure EditMemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SelectBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SelectBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Copymessagestoclipboard1Click(Sender: TObject);
    procedure imgModelErrorsClick(Sender: TObject);
    procedure InfoBrowserClick(Sender: TObject);
    procedure SyntaxcbClick(Sender: TObject);
    procedure FilterClick(Sender: TObject);
    procedure filterComboChange(Sender: TObject);
    procedure filterComboExit(Sender: TObject);
    procedure VariableTypescbClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fContext: TBoldElementTypeInfo;
    fSystemTypeInfo: TBoldSystemTypeInfo;
    fOclEvaluator: TBoldOcl;
    fVariables: TBoldExternalVariableList;
    fCompletions: TBoldOclCompletions;
    fLastCheck: TBoldOclCheckResult;
    fMRUExpressionList: TStringList;
    FOnExpressionChanged: TNotifyEvent;
    procedure SetOclExpr(Expr: String);
    function GetOclExpr: String;
    procedure SetContext(Context: TBoldElementTypeInfo);
    procedure SetSelection;
    procedure CleanParserMsg;
    procedure ApplyFilter;
    procedure UpdateVariables;
    function CheckOcl(Ocl: String): TBoldElementTypeInfo;
    function GetOclEvaluator: TBoldOcl;
    procedure WMGetMinMaxInfo(Var Msg: TWMGetMinMaxINfo); message WM_GETMINMAXINFO;
    procedure SetVariables(const Value: TBoldExternalVariableList);
    procedure AddExpressionToMRUList;
    procedure FetchMRUExpressionList;
    procedure MRUPopUpMenuClick(Sender: TObject);
    function HasVariables: boolean;
    procedure UpdateWidth;
    procedure SetOclEvaluator(const Value: TBoldOCL);
    procedure SaveFontSettings(const Font: TFont);
    procedure LoadFontSettings(const Font: TFont);
  public
    { Public declarations }
    ShowSyntaxErrors: Boolean;
    ShowDerived, ShowPersistent, ShowTransient: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OclEvaluator: TBoldOCL read GetOclEvaluator write SetOclEvaluator;
    property Context: TBoldElementTypeInfo read fContext write SetContext;
    property SystemTypeInfo: TBoldSystemTypeInfo read fSystemTypeInfo;
    property OclExpr: String read GetOclExpr write SetOclExpr;
    property Variables: TBoldExternalVariableList read fVariables write SetVariables;
    property OnExpressionChanged: TNotifyEvent read FOnExpressionChanged write FOnExpressionChanged;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  UITypes,  // Prevent H2443 for TFont.GetStyle

  BoldCoreConsts,
  BoldUtils,
  BoldDefs,
  BoldRegistry,
  BoldOclError,
  BoldSystem,
  BoldOclClasses,
  BoldBase;

{$R *.dfm}

const
  OCLInfoURL = 'https://www.omg.org/spec/OCL/2.0/PDF';
  OCLInfoLocal = 'doc\oclinfo.html';
  cMRUExpressionRegKey = '\MRU';
  cMRUExpressionRegKeyName = 'OCL Expression';
  cFontSettingRegKey = '\OCLEditorFont';
  cFontSettingRegKeyName = 'FontName';
  cFontSettingRegKeyStyle = 'FontStyle';
  cFontSettingRegKeySize = 'FontSize';
  cHiddenOCLVariables: array[0..3] of string = ('Nil', 'True', 'False', 'TimeStampNow');

type
  ModeType = (Arrow, Dot, unKnown);

procedure TBoldOclPropEditForm.CleanParserMsg;
begin
  ParserMessages.lines.clear;
end;

procedure TBoldOclPropEditForm.ClearClick(Sender: TObject);
begin
  EditMemo.Lines.Clear;
  EditMemoChange(Sender);
end;

constructor TBoldOclPropEditForm.Create(AOwner: TComponent);
begin
  inherited;
  ShowDerived := true;
  ShowPersistent := true;
  ShowTransient := true;
  ExprEditPageControl.ActivePage := ExpressionPage;
  SetFocusedControl(EditMemo);
end;

destructor TBoldOclPropEditForm.destroy;
begin
  inherited;
end;

function TBoldOclPropEditForm.GetOclEvaluator;
begin
  result := fOclEvaluator;
  if not Assigned(result) and Assigned(SystemTypeInfo) then
    result := SystemTypeInfo.Evaluator as TBoldOcl
end;

procedure TBoldOclPropEditForm.SetOclEvaluator(const Value: TBoldOCL);
begin
  fOclEvaluator := Value;
end;

procedure TBoldOclPropEditForm.SetContext(Context: TBoldElementTypeInfo);
begin
  fContext := Context;
  if assigned(Context) then
  begin
    fSystemTypeInfo := Context.SystemTypeInfo as TBoldSystemTypeInfo;
    Caption:= 'OCL expression editor - Context: ' + Context.AsString;
  end
  else
  begin
    Caption:= 'OCL expression editor - Context: <unknown>';
    CleanParserMsg;
    ParserMessages.lines.Add(sNoContextNoSupport);
  end;

  if (not assigned(SystemTypeInfo)) and (TBoldSystem.DefaultSystem <> nil) then
    fSystemTypeInfo := TBoldSystem.DefaultSystem.BoldType as tBoldSystemTypeInfo;

  filterCombo.ItemIndex := 0;

  UpdateWidth;

{
  if assigned( Model) then
  begin
    ContextCombo.Items.AddObject(Model.AsString, Model);
    for i := 0 To Model.RTInfo.TopSortedClasses.Count - 1 do
    begin
      ContextCombo.Items.AddObject(Model.RTInfo.TopSortedClasses[i].AsString, Model.RTInfo.TopSortedClasses[i]);
      for j := 0 to Model.RTInfo.TopSortedClasses[i].AllMembers.Count - 1 do
        ContextCombo.Items.AddObject(Model.RTInfo.TopSortedClasses[i].AllMembers[j].AsString,
                                      Model.RTInfo.TopSortedClasses[i].AllMembers[j]);
    end;
    for i := 0 to Model.AttributeTypes.Count - 1 do
      ContextCombo.Items.AddObject(Model.AttributeTypes[i].AsString, Model.AttributeTypes[i]);
    for i := 0 to Model.ListTypes.Count - 1 do
      if Assigned(TBoldListTypeInfo(Model.ListTypes[i]).Element) then
        ContextCombo.Items.AddObject(Model.ListTypes[i].AsString, Model.ListTypes[i]);

  end;
  for i := 0 to ContextCombo.Items.Count - 1 do
    if ContextCombo.Items.Objects[i] = Context then
      ContextCombo.ItemIndex := i;
}

  EditMemoChange(nil);
  imgModelErrors.visible := assigned(SystemTypeInfo) and (SystemTypeInfo.InitializationLog.count > 0);
end;

procedure TBoldOclPropEditForm.SetOclExpr(Expr: String);
var
  i: integer;
begin
  EditMemo.Text := Expr;
  i := Length(EditMemo.Text);
  EditMemo.SelStart := i - 2;
  EditMemoChange(nil);
end;

function TBoldOclPropEditForm.GetOclExpr: String;
begin
  result := EditMemo.Text;
end;

function TBoldOclPropEditForm.HasVariables: boolean;
begin
  result := (Assigned(fVariables) and (fVariables.count > 0)) or (Assigned(OclEvaluator) and (OclEvaluator.VariableCount > Length(cHiddenOCLVariables)));
end;

procedure TBoldOclPropEditForm.AddExpressionToMRUList;
var
  lRegistry: TBoldRegistry;
begin
  if fMRUExpressionList.IndexOf(EditMemo.Text) = -1 then
  begin
    lRegistry := TBoldRegistry.Create;
    try
      fMRUExpressionList.insert(0, EditMemo.Text);
      if fMRUExpressionList.Count > 10 then
      begin
        fMRUExpressionList.Delete(10);
      end;
      if lRegistry.OpenKey(cMRUExpressionRegKey) then
      begin
        lRegistry.WriteString(cMRUExpressionRegKeyName, fMRUExpressionList.CommaText);
        lRegistry.CloseKey;
      end;
    finally
      lRegistry.free;
    end;
  end;
end;

const
  cKindDigit: array[TBoldOclCompletionKind] of Char = ('1', '2', '3', '4', '5');

{ A line of the lists: the kind, which picks the icon, then the text. What is
  added to the expression is the text up to its first space. }
function CompletionLine(const ACompletion: TBoldOclCompletion): string;
begin
  Result := cKindDigit[ACompletion.Kind] + ' | ' + ACompletion.Name + ACompletion.Detail;
  if Pos(':', ACompletion.Detail) = 0 then
    Result := Result + ' ';
end;

procedure TBoldOclPropEditForm.ApplyFilter;
var
  i: Integer;
  vKindFilter: Integer;
begin
  // A line of the filter's list, or a text typed there
  case filterCombo.ItemIndex of
    -1: vKindFilter := ocfText;
    ocfAttributes..ocfOperations: vKindFilter := filterCombo.ItemIndex;
  else
    vKindFilter := ocfAll;
  end;
  for i := 0 to High(fCompletions) do
    if BoldOclCompletionPasses(fCompletions[i], vKindFilter, filterCombo.Text) then
      SelectBox.Items.AddObject(CompletionLine(fCompletions[i]), fCompletions[i].Symbol);
end;

procedure TBoldOclPropEditForm.LoadFontSettings(const Font: TFont);
var
  lRegistry: TBoldRegistry;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cFontSettingRegKey) then
    begin
      Font.Name := lRegistry.ReadString(cFontSettingRegKeyName, Font.Name);
      Font.Size := lRegistry.ReadInteger(cFontSettingRegKeySize, Font.Size);
      Font.Style := TFontStyles(Byte(lRegistry.ReadInteger(cFontSettingRegKeyStyle, Byte(Font.Style))));
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.Free;
  end;
end;

procedure TBoldOclPropEditForm.SaveFontSettings(const Font: TFont);
var
  lRegistry: TBoldRegistry;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cFontSettingRegKey) then
    begin
      lRegistry.WriteString(cFontSettingRegKeyName, Font.Name);
      lRegistry.WriteInteger(cFontSettingRegKeySize, Font.Size);
      lRegistry.WriteInteger(cFontSettingRegKeyStyle, Byte(Font.Style));
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.Free;
  end;
end;

procedure TBoldOclPropEditForm.btnSetFontClick(Sender: TObject);
begin
  FontDialog1.Font := EditMemo.Font;
  if FontDialog1.Execute then
  begin
    EditMemo.Font := FontDialog1.Font;
    SaveFontSettings(EditMemo.Font);
  end;
end;

{ The checking, the parser messages and the lists are BoldOclExpressionCheck's,
  which the Lazarus editor uses too (#124) }
function TBoldOclPropEditForm.CheckOcl(Ocl: String): TBoldElementTypeInfo;
begin
  fLastCheck := BoldCheckOclExpression(Ocl, Context, Variables, OclEvaluator);
  ParserMessages.Lines.Text := BoldOclCheckResultAsText(Ocl, fLastCheck, ShowSyntaxErrors);
  ParserMessages.SelStart := 0;
  ParserMessages.SelLength := 0;
  Result := fLastCheck.ResultType;
end;

procedure TBoldOclPropEditForm.RemoveLastClick(Sender: TObject);
begin
  if Trim(EditMemo.Text) = '' then
    exit;
  EditMemo.Text := BoldOclRemoveLast(EditMemo.Text);
end;

procedure TBoldOclPropEditForm.MRUPopUpMenuClick(Sender: TObject);
var
  lMenuItem: TMenuItem;
begin
  lMenuItem := (Sender as TMenuItem);
  EditMemo.Text := fMRUExpressionList[lMenuItem.Parent.IndexOf(lMenuItem)];
end;

procedure TBoldOclPropEditForm.SelectBoxDblClick(Sender: TObject);
var
  Addition: String;
  vListBox: TListBox;
begin
  if not assigned(Context) then
    exit;
  if not(Sender is TListBox) then
    exit;

  vListBox := Sender as TListBox;
  if vListBox.ItemIndex = -1 then
    vListBox.ItemIndex := 0;
  Addition := vListBox.Items[vListBox.ItemIndex];

  if pos(' | ', Addition) <> 0 then
    Delete(Addition, 1, Pos(' | ', Addition) + 2);

  Addition := Copy(Addition, 1, Pos(' ', Addition) - 1);

  if Pos(':', Addition) = Length(Addition) then
    Addition := Copy(Addition, 1, Length(Addition) - 1);

  EditMemo.Lines[EditMemo.Lines.Count - 1] :=
    EditMemo.Lines[EditMemo.Lines.Count - 1] +
    Addition;

  if filterCombo.ItemIndex = -1 then
    filterCombo.Clear;

  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.EditMemoEnter(Sender: TObject);
begin
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.SelectBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = BOLDCR then
    SelectBoxDblClick(Sender);
end;

procedure TBoldOclPropEditForm.EditMemoChange(Sender: TObject);
var
  Ocl: String;
  vOptions: TBoldOclMemberOptions;
begin
  if not assigned(Context) then
    exit;
  Ocl := trim(EditMemo.Text);
  vOptions := [];
  if ShowDerived then
    Include(vOptions, omoDerived);
  if ShowPersistent then
    Include(vOptions, omoPersistent);
  if ShowTransient then
    Include(vOptions, omoTransient);

  VariablesListBox.Items.BeginUpdate;
  SelectBox.Items.BeginUpdate;
  try
    SelectBox.Clear;
    CheckOcl(Ocl);
    BoldOclListCompletions(Ocl, fLastCheck, Context, vOptions, fCompletions, OclEvaluator);
    ApplyFilter;
    UpdateVariables;
  finally
    VariablesListBox.Items.EndUpdate;
    SelectBox.Items.EndUpdate;
  end;
  if Assigned(fOnExpressionChanged) then
    OnExpressionChanged(self);
end;

procedure TBoldOclPropEditForm.SyntaxcbClick(Sender: TObject);
begin
  ShowSyntaxErrors := syntaxcb.Checked;
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.FilterClick(Sender: TObject);
var
  OldIndex: Integer;
  OldTop: Integer;
begin
  OldIndex := SelectBox.ItemIndex;
  OldTop := SelectBox.TopIndex;
  ShowDerived := Derivedcb.Checked;
  ShowPersistent := Persistentcb.Checked;
  ShowTransient := Transientcb.Checked;
  EditMemoChange(Sender);
  SelectBox.ItemIndex := OldIndex;
  SelectBox.TopIndex := OldTop;
end;

procedure TBoldOclPropEditForm.UpdateVariables;
var
  i: integer;
  vItems: TBoldOclCompletions;
begin
  VariablesPageControl.Visible := HasVariables;
  VariablesListBox.Items.BeginUpdate;
  VariablesListBox.Clear;
  try
    BoldOclListVariables(Context, Variables, vItems, OclEvaluator);
    for i := 0 to High(vItems) do
      VariablesListBox.Items.Add(CompletionLine(vItems[i]));
  finally
    VariablesListBox.Items.EndUpdate;
  end;
end;

procedure TBoldOclPropEditForm.UpdateWidth;
begin
  if not visible then
  begin
    if HasVariables then
      Width := 900
    else
      Width := 720;
  end;
end;

procedure TBoldOclPropEditForm.VariableTypescbClick(Sender: TObject);
var
  OldIndex: Integer;
  OldTop: Integer;
begin
  OldIndex := VariablesListBox.ItemIndex;
  OldTop := VariablesListBox.TopIndex;
  EditMemoChange(Sender);
  VariablesListBox.ItemIndex := OldIndex;
  VariablesListBox.TopIndex := OldTop;
end;

procedure TBoldOclPropEditForm.SetSelection;
var
  vStart, vLength: Integer;
begin
  if (EditMemo.SelLength = 0) and
    BoldOclPlaceholderAt(EditMemo.Text, EditMemo.SelStart, vStart, vLength) then
  begin
    EditMemo.SelStart := vStart;
    EditMemo.SelLength := vLength;
  end;
end;

procedure TBoldOclPropEditForm.EditMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetSelection;
end;

procedure TBoldOclPropEditForm.FetchMRUExpressionList;
var
  lRegistry: TBoldRegistry;
  lIndex: Integer;
  lMenuItem: TMenuItem;
begin
  lRegistry := TBoldRegistry.Create;
  try
    if lRegistry.OpenKey(cMRUExpressionRegKey) then
    begin
      fMRUExpressionList.CommaText := lRegistry.ReadString(cMRUExpressionRegKeyName, '');
      lRegistry.CloseKey;
    end;
  finally
    lRegistry.free;
  end;
  for lIndex := 0 to fMRUExpressionList.Count - 1 do
  begin
    lMenuItem := TMenuItem.Create(Self);
    lMenuItem.Caption := '&' + IntToStr(lIndex+1) + ': ' + fMRUExpressionList[lIndex];
    lMenuItem.OnClick := MRUPopUpMenuClick;
    MRUPopupMenu.Items.Add(lMenuItem);
  end;
end;

procedure TBoldOclPropEditForm.filterComboChange(Sender: TObject);
begin
  EditMemoChange(Sender);
end;

procedure TBoldOclPropEditForm.filterComboExit(Sender: TObject);
begin
  if (FilterCombo.ItemIndex = -1) and (FilterCombo.Text = '') then
  begin
    FilterCombo.ItemIndex := 0;
    EditMemoChange(Sender);
  end;
end;

procedure TBoldOclPropEditForm.FormCreate(Sender: TObject);
begin
  fMRUExpressionList := TStringList.Create;
  FetchMRUExpressionList;
  LoadFontSettings(EditMemo.Font);
end;

procedure TBoldOclPropEditForm.FormDestroy(Sender: TObject);
begin
  fMRUExpressionList.free;
end;

procedure TBoldOclPropEditForm.EditMemoKeyPress(Sender: TObject; var Key: Char);
begin
  SetSelection;
  if Key = BOLDESC then
    ModalResult := mrCancel;
end;

procedure TBoldOclPropEditForm.OKClick(Sender: TObject);
begin
  Modalresult := mrOK;
  AddExpressionToMRUList;
end;

procedure TBoldOclPropEditForm.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TBoldOclPropEditForm.EditMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   SetSelection;
end;

{
procedure TBoldOclPropEditForm.AddVarClick(Sender: TObject);
begin
   VariableList.Items.Add('NewVariable');
   VariableList.ItemIndex := VariableList.Items.Count - 1;
   VariableNameEdit.Text := VariableList.Items[VariableList.ItemIndex];
   VariableNameEdit.AutoSelect := true;
   SetFocusedControl(VariableNameEdit);
end;

procedure TBoldOclPropEditForm.VariablenameEditChange(Sender: TObject);
begin
  VariableNameEdit.AutoSelect := false;
end;

procedure TBoldOclPropEditForm.VariablenameEditExit(Sender: TObject);
begin
  if not VariableNameEdit.readOnly then
    VariableList.Items[VariableList.ItemIndex] := VariableNameEdit.Text;
end;

procedure TBoldOclPropEditForm.VariablenameEditEnter(Sender: TObject);
begin
  variableNameEdit.ReadOnly := variableList.ItemIndex = -1;
end;

procedure TBoldOclPropEditForm.VariableListClick(Sender: TObject);
begin
  if not (variableList.ItemIndex = -1) then
    VariableNameEdit.Text := VariableList.Items[VariableList.ItemIndex];
end;

procedure TBoldOclPropEditForm.RemoveVariableClick(Sender: TObject);
var
  OldIndex: Integer;
begin
  if VariableList.ItemIndex <> -1 then
  begin
    OldIndex := VariableList.ItemIndex;
    VariableList.Items.Delete(VariableList.ItemIndex);
    if OldIndex < VariableList.Items.Count then
      VariableList.ItemIndex := OldIndex
    else if VariableList.Items.Count > 0 then
      VariableList.ItemIndex := OldIndex - 1;
  end;
end;

procedure TBoldOclPropEditForm.ContextComboChange(Sender: TObject);
begin
  Context := TBoldElementTypeInfo(ContextCombo.Items.Objects[ContextCombo.ItemIndex]);
end;
}

procedure TBoldOclPropEditForm.SelectBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  StringfromSelect: String;
  s: string;
  r: TRect;
  vXOffset: integer;
begin
  with (Control as TListBox) do
  begin
    Canvas.FillRect(Rect);
    StringFromSelect := (Control as TListBox).items[Index];
    case StringFromSelect[1] of
      '1': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 0);
      '2': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 2);
      '3': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 3);
      '4': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 1);
      '5': ImageList1.Draw(Canvas, Rect.Left + 2, Rect.Top, 4);
    end;
    if pos(' | ', StringFromSelect) <> 0 then
      Delete(StringFromSelect, 1, Pos(' | ', StringFromSelect) + 2);

    vXOffset := 2 + ImageList1.Width;
    if not ((odFocused in State) or (odSelected in State)) then
    begin
      s := StringFromSelect;

      if pos(':', s) <> 0 then
        Delete(s, Pos(':', s), MaxInt);

      if pos(' ', s) <> 0 then
        Delete(s, Pos(' ', s), MaxInt);

      if pos('(', s) <> 0 then
        Delete(s, Pos('(', s), MaxInt);

      r := Rect;
      r.Left := r.Left + vXOffset;
      Canvas.Font.Color := clBlack;
      DrawText(Canvas.Handle,
        PChar(S),
        Length(S),
        r,
        DT_LEFT or DT_WORDBREAK or DT_CALCRECT);

      DrawText(Canvas.Handle,
        PChar(S),
        Length(S),
        r,
        DT_LEFT or DT_WORDBREAK);

      vXOffset := r.Right;
      Canvas.Font.Color := $00A0A0A0;;
      StringFromSelect := Copy(StringfromSelect, Length(s)+1, MaxInt);
    end;

    Canvas.TextOut(Rect.Left + vXOffset, Rect.Top, StringFromSelect);
	end;
end;

procedure TBoldOclPropEditForm.WMGetMinMaxInfo(Var Msg: TWMGetMinMaxINfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize.x := 525;
    ptMintrackSize.y := 264;
  end;
  Msg.Result := 0;
end;

procedure TBoldOclPropEditForm.SelectBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Symbol: TObject;
begin
  if (Key = 112) and (SelectBox.ItemIndex <> -1) then
  begin
    Symbol := SelectBox.Items.Objects[SelectBox.ItemIndex];
    if symbol is TBoldOclSymbol then
    begin
      SelectBox.HelpContext := (Symbol as TBoldOclSymbol).HelpContext;
    end
    else
      SelectBox.HelpContext := 0;
  end;
end;

procedure TBoldOclPropEditForm.SetVariables(const Value: TBoldExternalVariableList);
begin
  fVariables := Value;
  EditMemoChange(nil);
  UpdateWidth;
end;

procedure TBoldOclPropEditForm.Copymessagestoclipboard1Click(Sender: TObject);
var
  s: string;
begin
  if Assigned(Context) then
    s := 'Context: ' + Context.ExpressionName + BOLDCRLF
  else
    s := '';
  ClipBoard.AsText := s +
                      'OCL Expression: ' + BOLDCRLF +
                      EditMemo.Text + BOLDCRLF + BOLDCRLF +
                      'Parser messages: ' + BOLDCRLF +
                      ParserMessages.lines.text;

end;

procedure TBoldOclPropEditForm.imgModelErrorsClick(Sender: TObject);
begin
  Showmessage(sModelContainsErrors + BoldCRLF + BoldCRLF+ SystemTypeInfo.InitializationLog.text);
end;

procedure TBoldOclPropEditForm.InfoBrowserClick(Sender: TObject);
var
  URL: String;
begin
  if (GetBoldBasePath <> '') and fileexists(GetBoldBasePath+'\'+OCLInfoLocal) then
    URL := GetBoldBasePath+'\'+OCLInfoLocal
  else
    URL := OCLInfoURL;

  ShellExecute(0, 'open', PChar(URL), '', '', SW_SHOWMAXIMIZED);
end;

end.