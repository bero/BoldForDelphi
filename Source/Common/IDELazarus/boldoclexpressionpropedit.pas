
{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionPropEdit;

{ The Lazarus property editor of an OCL expression (#117): the "..." button
  of Expression in the Object Inspector opens Bold's OCL editor, the LCL
  dialog of BoldOclExpressionDialogLCL (in the runtime package BoldLCL, so an
  application can open it too), on the type the expression is evaluated in. }

interface

uses
  Classes,
  PropEdits,
  BoldElements,
  BoldOclExpressionDialogLCL;

type
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

uses
  SysUtils,
  TypInfo,
  Controls,
  BoldControlPack;

resourcestring
  sNotAnExpressionOwner = '%s has no OCL expression to edit';

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
