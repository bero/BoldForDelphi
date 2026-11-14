
{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionCheck;

{ Checks an OCL expression against the model, for the Lazarus property editor
  of Expression (#117): the type the expression is evaluated in comes from the
  component that owns the property, the check is the one the OCL evaluator
  does when it parses, and the answer is the type of the result or the error
  and where in the expression it is. No LCL and no IDE unit, so
  packages/Lazarus/tests/BoldRegistrationCheck tests it without the IDE. }

interface

uses
  Classes,
  BoldElements;

type
  TBoldOclCheckResult = record
    { False: there is no context type, so nothing was checked }
    Checked: Boolean;
    Valid: Boolean;
    ContextName: string;
    { The type of the result of a valid expression }
    ResultTypeName: string;
    { Of an expression that is not valid; the position is the offset of the
      character in the expression, from 0, or -1 when the error has none }
    ErrorMessage: string;
    ErrorPosition: Integer;
  end;

{ The type an expression of APersistent is evaluated in, and its variables:
  a handle with an expression (IBoldOCLComponent) or a follower controller,
  as the BoldProperties of a TBoldDataSet field. False when APersistent is
  neither; AContext is nil when the component is not connected to a model. }
function BoldOclContextOf(APersistent: TPersistent; out AContext: TBoldElementTypeInfo;
  out AVariables: TBoldExternalVariableList): Boolean;

function BoldCheckOclExpression(const AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList): TBoldOclCheckResult;

{ One or two lines for the user: the result type, or the error with the
  expression and a marker under the place }
function BoldOclCheckResultAsText(const AExpression: string; const AResult: TBoldOclCheckResult): string;

implementation

uses
  SysUtils,
  BoldControlPack;

resourcestring
  sNoContext = 'No context: the component is not connected to a model, so the expression is not checked';
  sResultType = 'Result type: %s';
  sResultTypeUnknown = 'Result type: unknown';
  sErrorAt = 'Error at position %d: %s';
  sError = 'Error: %s';

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

{ The evaluator's messages start with the position and a colon }
procedure SplitMessage(const AMessage: string; out APosition: Integer; out AText: string);
var
  Colon: Integer;
begin
  APosition := -1;
  AText := AMessage;
  Colon := Pos(':', AMessage);
  if (Colon > 1) and TryStrToInt(Copy(AMessage, 1, Colon - 1), APosition) then
    AText := Trim(Copy(AMessage, Colon + 1, MaxInt))
  else
    APosition := -1;
end;

function BoldCheckOclExpression(const AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList): TBoldOclCheckResult;
var
  ResultType: TBoldElementTypeInfo;
begin
  Result.Checked := False;
  Result.Valid := False;
  Result.ContextName := '';
  Result.ResultTypeName := '';
  Result.ErrorMessage := '';
  Result.ErrorPosition := -1;
  if not Assigned(AContext) or not Assigned(AContext.Evaluator) then
    Exit;
  Result.Checked := True;
  Result.ContextName := AContext.AsString;
  try
    // An empty expression is the context itself
    ResultType := AContext.Evaluator.ExpressionType(AExpression, AContext, True, AVariables);
    Result.Valid := True;
    if Assigned(ResultType) then
      Result.ResultTypeName := ResultType.AsString;
  except
    // EBoldOCLAbort is an EAbort, EBoldOCLError an EBold
    on E: Exception do
      SplitMessage(E.Message, Result.ErrorPosition, Result.ErrorMessage);
  end;
end;

function BoldOclCheckResultAsText(const AExpression: string; const AResult: TBoldOclCheckResult): string;
begin
  if not AResult.Checked then
    Result := sNoContext
  else if AResult.Valid then
  begin
    if AResult.ResultTypeName <> '' then
      Result := Format(sResultType, [AResult.ResultTypeName])
    else
      Result := sResultTypeUnknown;
  end
  else if AResult.ErrorPosition >= 0 then
    Result := Format(sErrorAt, [AResult.ErrorPosition, AResult.ErrorMessage])
  else
    Result := Format(sError, [AResult.ErrorMessage]);
end;

end.
