
{ Global compiler directives }
{$include bold.inc}
unit BoldOclExpressionCheck;

{ What an OCL editor needs, without its form: checks an expression against
  the type it is evaluated in and gives the parser messages (the type of the
  result, or the expression with a pointer at the error), lists what can
  follow the expression (members, classes, operations) and the OCL variables,
  and has Remove Last and the search for an argument placeholder.

  One copy for the two editors (#124): the VCL form BoldOclPropEditor and the
  Lazarus dialog BoldOclExpressionPropEdit (#117). No VCL, LCL or IDE unit. }

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
    { The type of the result of a valid expression; of an empty expression
      the context }
    ResultType: TBoldElementTypeInfo;
    ResultTypeName: string;
    { Of an expression that is not valid. An OCL error has a position: the
      offset of the character in the expression, from 0. Another exception
      has -1. ErrorMessage is the text without the position the evaluator
      puts before it, RawErrorMessage with it, as the editors show it. }
    ErrorMessage: string;
    RawErrorMessage: string;
    ErrorPosition: Integer;
    IsOclError: Boolean;
    IsInternalError: Boolean;
    { The error is one of the parser's own (SSYacc, SSLex) }
    IsSyntaxError: Boolean;
  end;

  { What can follow an expression, as Delphi's OCL editor lists it }
  TBoldOclCompletionKind = (ockAttribute, ockSingleLink, ockMultiLink, ockType, ockOperation);
  TBoldOclCompletion = record
    Kind: TBoldOclCompletionKind;
    { What is added to the expression: '.name', '->select(«Boolean»)' }
    InsertText: string;
    { The line of the list in two parts: the name, and its arguments and type }
    Name: string;
    Detail: string;
    { Of an operation: its TBoldOclSymbol }
    Symbol: TObject;
  end;
  TBoldOclCompletions = array of TBoldOclCompletion;

  TBoldOclMemberOption = (omoDerived, omoPersistent, omoTransient);
  TBoldOclMemberOptions = set of TBoldOclMemberOption;

const
  { AKindFilter of BoldOclCompletionPasses: the lines of the editor's filter
    list, or a text typed there }
  ocfAll = 0;
  ocfAttributes = 1;
  ocfSingleLinks = 2;
  ocfMultiLinks = 3;
  ocfOperations = 4;
  ocfText = -1;

{ AEvaluator: another evaluator than the one of the context's system }
function BoldCheckOclExpression(const AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList; AEvaluator: TBoldEvaluator = nil): TBoldOclCheckResult;

{ The parser messages of Delphi's OCL editor: 'Syntax is OK' and the type, or
  the expression with a pointer under the error, and the message. A parser
  error of SSYacc or SSLex is shown only with AShowSyntaxErrors. }
function BoldOclCheckResultAsText(const AExpression: string; const AResult: TBoldOclCheckResult;
  AShowSyntaxErrors: Boolean = False): string;

{ The members of the type of the expression (of its elements, for a list),
  or the classes for the system, and the operations that take that type.
  Nothing when the expression is not valid. }
procedure BoldOclListCompletions(const AExpression: string; const ACheck: TBoldOclCheckResult;
  AContext: TBoldElementTypeInfo; AOptions: TBoldOclMemberOptions; out AItems: TBoldOclCompletions;
  AEvaluator: TBoldEvaluator = nil);
function BoldOclCompletionPasses(const AItem: TBoldOclCompletion; AKindFilter: Integer;
  const AFilterText: string): Boolean;
{ The variables the expression can use: the component's, and the evaluator's
  own except Nil, True, False and TimeStampNow }
procedure BoldOclListVariables(AContext: TBoldElementTypeInfo; AVariables: TBoldExternalVariableList;
  out AItems: TBoldOclCompletions; AEvaluator: TBoldEvaluator = nil);

{ The expression without its last step: back to the last '.', '->' or space }
function BoldOclRemoveLast(const AExpression: string): string;
{ An argument placeholder «...» around the caret (ACaret = characters before
  it), to select it as a whole. AStart counts from 0. }
function BoldOclPlaceholderAt(const AText: string; ACaret: Integer; out AStart, ALength: Integer): Boolean;

implementation

uses
  SysUtils,
  BoldCoreConsts,
  BoldOclError,
  BoldSystemRT,
  BoldOcl,
  BoldOclClasses;

const
  cHiddenOclVariables: array[0..3] of string = ('Nil', 'True', 'False', 'TimeStampNow');
  cPlaceholderOpen = '«';
  cPlaceholderClose = '»';

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

function EvaluatorOf(AContext: TBoldElementTypeInfo; AEvaluator: TBoldEvaluator): TBoldEvaluator;
begin
  Result := AEvaluator;
  if not Assigned(Result) and Assigned(AContext) then
    Result := AContext.Evaluator;
end;

function BoldCheckOclExpression(const AExpression: string; AContext: TBoldElementTypeInfo;
  AVariables: TBoldExternalVariableList; AEvaluator: TBoldEvaluator): TBoldOclCheckResult;
var
  Evaluator: TBoldEvaluator;

  procedure OclError(E: Exception; APosition: Integer);
  var
    Position: Integer;
  begin
    Result.IsOclError := True;
    Result.ErrorPosition := APosition;
    Result.RawErrorMessage := E.Message;
    Result.IsSyntaxError := (Pos('SSYacc', E.Message) > 0) or (Pos('SSLex', E.Message) > 0);
    SplitMessage(E.Message, Position, Result.ErrorMessage);
  end;

begin
  Result.Checked := False;
  Result.Valid := False;
  Result.ContextName := '';
  Result.ResultType := nil;
  Result.ResultTypeName := '';
  Result.ErrorMessage := '';
  Result.RawErrorMessage := '';
  Result.ErrorPosition := -1;
  Result.IsOclError := False;
  Result.IsInternalError := False;
  Result.IsSyntaxError := False;
  Evaluator := EvaluatorOf(AContext, AEvaluator);
  if not Assigned(AContext) or not Assigned(Evaluator) then
    Exit;
  Result.Checked := True;
  Result.ContextName := AContext.AsString;
  try
    // An empty expression is the context itself
    Result.ResultType := Evaluator.ExpressionType(AExpression, AContext, True, AVariables);
    Result.Valid := True;
    if Assigned(Result.ResultType) then
      Result.ResultTypeName := Result.ResultType.AsString;
  except
    // The evaluator has given the exception its position (FixError)
    on E: EBoldOclAbort do
    begin
      Result.ResultType := nil;
      E.FixError;
      OclError(E, E.Position);
    end;
    on E: EBoldOclError do
    begin
      Result.ResultType := nil;
      E.FixError;
      OclError(E, E.Position);
      Result.IsInternalError := E is EBoldOclInternalError;
    end;
    on E: Exception do
    begin
      Result.ResultType := nil;
      Result.ErrorMessage := E.Message;
      Result.RawErrorMessage := E.Message;
    end;
  end;
end;

{ The expression, a pointer under the place of the error in the line that
  has it, and the message: what Delphi's editor shows }
function ErrorAsText(const AExpression: string; const AResult: TBoldOclCheckResult): string;
var
  Lines: TStringList;
  Line, Offset: Integer;
  Pointer: string;
begin
  Lines := TStringList.Create;
  try
    if AResult.IsInternalError then
      Lines.Text := 'Internal Error: ' + AExpression
    else
      Lines.Text := AExpression;
    if (AResult.ErrorPosition >= 0) and (Lines.Count > 0) then
    begin
      Line := 0;
      Offset := AResult.ErrorPosition;
      // Each line break of the expression counts as its characters
      while (Line < Lines.Count - 1) and (Offset > Length(Lines[Line])) do
      begin
        Dec(Offset, Length(Lines[Line]) + Length(sLineBreak));
        Inc(Line);
      end;
      if Offset < 0 then
        Offset := 0;
      if Offset >= 10 then
        Pointer := StringOfChar(' ', Offset - 7) + '-------^'
      else
        Pointer := StringOfChar(' ', Offset) + '^-------';
      Lines.Insert(Line + 1, Pointer);
    end;
    Lines.Add(AResult.RawErrorMessage);
    Result := TrimRight(Lines.Text);
  finally
    Lines.Free;
  end;
end;

function BoldOclCheckResultAsText(const AExpression: string; const AResult: TBoldOclCheckResult;
  AShowSyntaxErrors: Boolean): string;
begin
  if not AResult.Checked then
    Result := sNoContextNoSupport
  else if AResult.Valid then
  begin
    if AExpression = '' then
      Result := sEmptyExpression
    else if AResult.ResultTypeName <> '' then
      Result := sSyntaxOK + sLineBreak + sLineBreak + sCurrentTypeIsX + AResult.ResultTypeName
    else
      Result := sSyntaxOK + sLineBreak + sLineBreak + sCurrentTypeIsUnknown;
  end
  else if not AResult.IsOclError then
    Result := AResult.RawErrorMessage
  else if AResult.IsSyntaxError and not AShowSyntaxErrors then
    Result := sSyntaxError
  else
    Result := ErrorAsText(AExpression, AResult);
end;

procedure AddCompletion(var AItems: TBoldOclCompletions; AKind: TBoldOclCompletionKind;
  const AInsertText, AName, ADetail: string; ASymbol: TObject = nil);
begin
  SetLength(AItems, Length(AItems) + 1);
  with AItems[High(AItems)] do
  begin
    Kind := AKind;
    InsertText := AInsertText;
    Name := AName;
    Detail := ADetail;
    Symbol := ASymbol;
  end;
end;

procedure ListMembers(const AExpression: string; AType: TBoldElementTypeInfo;
  AOptions: TBoldOclMemberOptions; var AItems: TBoldOclCompletions);
var
  i: Integer;
  ClassInfo: TBoldClassTypeInfo;
  SystemInfo: TBoldSystemTypeInfo;
  Member: TBoldMemberRTInfo;
  Role: TBoldRoleRTInfo;
  Prefix, Name: string;
begin
  if AType is TBoldListTypeInfo then
    AType := TBoldListTypeInfo(AType).ListElementTypeInfo;
  if AExpression <> '' then
    Prefix := '.'
  else
    Prefix := '';
  if AType is TBoldClassTypeInfo then
  begin
    ClassInfo := TBoldClassTypeInfo(AType);
    for i := 0 to ClassInfo.AllMembersCount - 1 do
    begin
      Member := ClassInfo.AllMembers[i];
      if not (omoDerived in AOptions) and Member.IsDerived then
        Continue;
      if not (omoPersistent in AOptions) and Member.Persistent then
        Continue;
      if not (omoTransient in AOptions) and not (Member.Persistent or Member.IsDerived) then
        Continue;
      Name := Prefix + Member.ExpressionName;
      if Member is TBoldAttributeRTInfo then
        AddCompletion(AItems, ockAttribute, Name, Name, ': ' + Member.BoldType.ExpressionName)
      else if Member is TBoldRoleRTInfo then
      begin
        Role := TBoldRoleRTInfo(Member);
        if Role.IsNavigable then
        begin
          if Role.IsMultiRole then
            AddCompletion(AItems, ockMultiLink, Name, Name,
              ': List<' + Role.ClassTypeInfoOfOtherEnd.ExpressionName + '>')
          else
            AddCompletion(AItems, ockSingleLink, Name, Name, ': ' + Role.ClassTypeInfoOfOtherEnd.ExpressionName);
        end;
      end;
    end;
  end
  else if AType is TBoldSystemTypeInfo then
  begin
    SystemInfo := TBoldSystemTypeInfo(AType);
    for i := 0 to SystemInfo.TopSortedClasses.Count - 1 do
      with SystemInfo.TopSortedClasses[i] do
        AddCompletion(AItems, ockType, ExpressionName, ExpressionName, ': ' + DelphiName);
  end;
end;

procedure ListOperations(AType: TBoldElementTypeInfo; AEvaluator: TBoldOcl; var AItems: TBoldOclCompletions);
var
  i, j: Integer;
  Symbol: TBoldOclSymbol;
  Name, Arguments, Detail: string;
  ArgumentType: TBoldElementTypeInfo;
begin
  if not Assigned(AEvaluator.SymbolTable) then
    Exit;
  for i := 0 to AEvaluator.SymbolTable.Count - 1 do
  begin
    Symbol := AEvaluator.SymbolTable.Symbols[i];
    if not Symbol.IsPostfix then
      Continue;
    ArgumentType := Symbol.FormalArguments[0];
    if Assigned(ArgumentType) and not AType.ConformsTo(ArgumentType) and
      not ((AType is TBoldListTypeInfo) and Assigned(TBoldListTypeInfo(AType).ListElementTypeInfo) and
        TBoldListTypeInfo(AType).ListElementTypeInfo.ConformsTo(ArgumentType)) then
      Continue;
    if Symbol.IsDotNotation then
      Name := '.' + Symbol.SymbolName
    else
      Name := '->' + Symbol.SymbolName;
    Arguments := '';
    if Symbol.NumberOfArgs > 1 then
    begin
      for j := 1 to Symbol.NumberOfArgs - 1 do
      begin
        ArgumentType := Symbol.FormalArguments[j];
        if j > 1 then
          Arguments := Arguments + ',';
        if not Assigned(ArgumentType) then
          Arguments := Arguments + cPlaceholderOpen + 'AnyArg' + cPlaceholderClose
        else if ArgumentType is TBoldListTypeInfo then
        begin
          if Assigned(TBoldListTypeInfo(ArgumentType).ListElementTypeInfo) then
            Arguments := Arguments + cPlaceholderOpen + 'List<' +
              TBoldListTypeInfo(ArgumentType).ListElementTypeInfo.ExpressionName + '>' + cPlaceholderClose
          else
            Arguments := Arguments + cPlaceholderOpen + 'List<AnyArg>' + cPlaceholderClose;
        end
        else
          Arguments := Arguments + cPlaceholderOpen + ArgumentType.ExpressionName + cPlaceholderClose;
      end;
      Arguments := '(' + Arguments + ')';
    end;
    if Assigned(Symbol.ResultType) then
      Detail := Arguments + ' : ' + Symbol.ResultType.ExpressionName
    else
      Detail := Arguments;
    AddCompletion(AItems, ockOperation, Name + Arguments, Name, Detail, Symbol);
  end;
end;

procedure BoldOclListCompletions(const AExpression: string; const ACheck: TBoldOclCheckResult;
  AContext: TBoldElementTypeInfo; AOptions: TBoldOclMemberOptions; out AItems: TBoldOclCompletions;
  AEvaluator: TBoldEvaluator);
var
  Evaluator: TBoldEvaluator;
begin
  AItems := nil;
  if not ACheck.Valid or not Assigned(ACheck.ResultType) then
    Exit;
  ListMembers(AExpression, ACheck.ResultType, AOptions, AItems);
  Evaluator := EvaluatorOf(AContext, AEvaluator);
  if (AExpression <> '') and (Evaluator is TBoldOcl) then
    ListOperations(ACheck.ResultType, TBoldOcl(Evaluator), AItems);
end;

function BoldOclCompletionPasses(const AItem: TBoldOclCompletion; AKindFilter: Integer;
  const AFilterText: string): Boolean;
begin
  case AKindFilter of
    ocfAttributes: Result := AItem.Kind = ockAttribute;
    ocfSingleLinks: Result := AItem.Kind = ockSingleLink;
    ocfMultiLinks: Result := AItem.Kind = ockMultiLink;
    ocfOperations: Result := AItem.Kind = ockOperation;
    ocfText: Result := (Trim(AFilterText) = '') or
      (Pos(LowerCase(Trim(AFilterText)), LowerCase(AItem.Name + AItem.Detail)) > 0);
  else
    Result := True;
  end;
end;

function IsHiddenVariable(const AName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(cHiddenOclVariables) to High(cHiddenOclVariables) do
    if SameText(AName, cHiddenOclVariables[i]) then
      Exit;
  Result := False;
end;

procedure BoldOclListVariables(AContext: TBoldElementTypeInfo; AVariables: TBoldExternalVariableList;
  out AItems: TBoldOclCompletions; AEvaluator: TBoldEvaluator);

  procedure AddVariable(const AName: string; AType: TBoldElementTypeInfo);
  var
    Kind: TBoldOclCompletionKind;
  begin
    if not Assigned(AType) then
    begin
      AddCompletion(AItems, ockOperation, AName, AName, ': Nil');
      Exit;
    end;
    if AType is TBoldListTypeInfo then
      Kind := ockMultiLink
    else if AType is TBoldAttributeTypeInfo then
      Kind := ockAttribute
    else if AType is TBoldClassTypeInfo then
      Kind := ockSingleLink
    else if AType is TBoldSystemTypeInfo then
      Kind := ockType
    else
      Kind := ockOperation;
    AddCompletion(AItems, Kind, AName, AName, ': ' + AType.ExpressionName);
  end;

var
  i: Integer;
  Evaluator: TBoldOcl;
  Binding: TBoldOclVariableBinding;
begin
  AItems := nil;
  if Assigned(AVariables) then
    for i := 0 to AVariables.Count - 1 do
      AddVariable(AVariables[i].Name, AVariables[i].ValueType);
  if EvaluatorOf(AContext, AEvaluator) is TBoldOcl then
  begin
    Evaluator := TBoldOcl(EvaluatorOf(AContext, AEvaluator));
    for i := 0 to Evaluator.VariableCount - 1 do
    begin
      Binding := Evaluator.Variables[i] as TBoldOclVariableBinding;
      if not IsHiddenVariable(Binding.VariableName) then
        AddVariable(Binding.VariableName, Binding.BoldType);
    end;
  end;
end;

function BoldOclRemoveLast(const AExpression: string): string;
begin
  Result := Trim(AExpression);
  while (Result <> '') and (Result[Length(Result)] <> '.') and (Result[Length(Result)] <> ' ') and
    (Copy(Result, Length(Result) - 1, 2) <> '->') do
    Delete(Result, Length(Result), 1);
  if Result <> '' then
  begin
    if Result[Length(Result)] = '.' then
      Delete(Result, Length(Result), 1)
    else if Copy(Result, Length(Result) - 1, 2) = '->' then
      Delete(Result, Length(Result) - 1, 2);
  end;
  Result := Trim(Result);
end;

function BoldOclPlaceholderAt(const AText: string; ACaret: Integer; out AStart, ALength: Integer): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  AStart := 0;
  ALength := 0;
  i := ACaret;
  if i > Length(AText) then
    i := Length(AText);
  while (i > 0) and (AText[i] <> cPlaceholderOpen) and (AText[i] <> cPlaceholderClose) do
    Dec(i);
  if (i = 0) or (AText[i] = cPlaceholderClose) then
    Exit;
  j := i + 1;
  while (j <= Length(AText)) and (AText[j] <> cPlaceholderOpen) and (AText[j] <> cPlaceholderClose) do
    Inc(j);
  if (j > Length(AText)) or (AText[j] = cPlaceholderOpen) then
    Exit;
  AStart := i - 1;
  ALength := j - i + 1;
  Result := True;
end;

end.
