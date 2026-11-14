
{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionPropEdit;

{ The Lazarus property editor of an OCL expression (#117): the "..." button
  of Expression in the Object Inspector opens a dialog with the expression,
  the type it is evaluated in, and, as the expression is typed, the type of
  its result or the error and its position. BoldOclExpressionCheck does the
  checking. It stands in for Delphi's OCL editor (BoldOclPropEditor), whose
  lists of members and operations it does not have. }

interface

uses
  Classes,
  Forms,
  StdCtrls,
  ButtonPanel,
  PropEdits,
  BoldElements,
  BoldOclExpressionCheck;

type
  { TBoldOclExpressionDialog }
  TBoldOclExpressionDialog = class(TForm)
  private
    fContext: TBoldElementTypeInfo;
    fVariables: TBoldExternalVariableList;
    fExpressionMemo: TMemo;
    fStatusMemo: TMemo;
    fButtons: TButtonPanel;
    fLastCheck: TBoldOclCheckResult;
    function GetExpression: string;
    function GetStatusText: string;
    procedure SetExpression(const AValue: string);
    procedure ExpressionChanged(Sender: TObject);
    procedure UpdateCheck;
  public
    { AContext may be nil: the expression can be edited but is not checked }
    constructor CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
      AVariables: TBoldExternalVariableList);
    property Expression: string read GetExpression write SetExpression;
    property LastCheck: TBoldOclCheckResult read fLastCheck;
    { What the dialog shows under the expression }
    property StatusText: string read GetStatusText;
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

implementation

uses
  SysUtils,
  TypInfo,
  Controls;

resourcestring
  sCaption = 'OCL expression';
  sCaptionWithContext = 'OCL expression - context: %s';
  sNotAnExpressionOwner = '%s has no OCL expression to edit';

{ TBoldOclExpressionDialog }

constructor TBoldOclExpressionDialog.CreateDialog(AOwner: TComponent; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList);
begin
  inherited CreateNew(AOwner);
  fContext := AContext;
  fVariables := AVariables;
  if Assigned(AContext) then
    Caption := Format(sCaptionWithContext, [AContext.AsString])
  else
    Caption := sCaption;
  Position := poScreenCenter;
  Width := 560;
  Height := 300;
  Constraints.MinWidth := 320;
  Constraints.MinHeight := 200;

  fButtons := TButtonPanel.Create(Self);
  fButtons.Parent := Self;
  fButtons.ShowButtons := [pbOK, pbCancel];

  fStatusMemo := TMemo.Create(Self);
  fStatusMemo.Parent := Self;
  fStatusMemo.Align := alBottom;
  fStatusMemo.Height := 64;
  fStatusMemo.ReadOnly := True;
  fStatusMemo.TabStop := False;
  fStatusMemo.ParentColor := True;
  fStatusMemo.BorderSpacing.Around := 6;

  fExpressionMemo := TMemo.Create(Self);
  fExpressionMemo.Parent := Self;
  fExpressionMemo.Align := alClient;
  fExpressionMemo.BorderSpacing.Around := 6;
  fExpressionMemo.WordWrap := True;
  fExpressionMemo.ScrollBars := ssAutoVertical;
  fExpressionMemo.OnChange := ExpressionChanged;
  ActiveControl := fExpressionMemo;

  UpdateCheck;
end;

function TBoldOclExpressionDialog.GetExpression: string;
begin
  // The memo ends its text with a line break
  Result := TrimRight(fExpressionMemo.Lines.Text);
end;

procedure TBoldOclExpressionDialog.SetExpression(const AValue: string);
begin
  fExpressionMemo.Lines.Text := AValue;
  // Not left to OnChange, which a memo without a window handle does not fire
  UpdateCheck;
end;

function TBoldOclExpressionDialog.GetStatusText: string;
begin
  Result := TrimRight(fStatusMemo.Lines.Text);
end;

procedure TBoldOclExpressionDialog.ExpressionChanged(Sender: TObject);
begin
  UpdateCheck;
end;

procedure TBoldOclExpressionDialog.UpdateCheck;
begin
  fLastCheck := BoldCheckOclExpression(Expression, fContext, fVariables);
  fStatusMemo.Lines.Text := BoldOclCheckResultAsText(Expression, fLastCheck);
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
