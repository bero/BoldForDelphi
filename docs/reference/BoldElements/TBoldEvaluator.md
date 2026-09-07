# TBoldEvaluator

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldEvaluator = class(TBoldMemoryManagedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. TBoldEvaluator
4. **Direct subclasses**
5. `TBoldRTEvaluator`

## Description

An implementation of Bold can contain one or more evaluators for expressions. The current implementation contains an evaluator for OCL

## Methods

| Name | Summary | Notes |
|---|---|---|
| [DefineVariable](#definevariable) |  | abstract |
| [Evaluate](#evaluate) |  | abstract |
| [ExpressionType](#expressiontype) |  | abstract |
| [SetLookupOclDefinition](#setlookupocldefinition) |  | abstract |

### DefineVariable

```delphi
procedure DefineVariable(const VariableName: string; VarValue: TBoldElement; VariableType: TBoldElementTypeInfo; OwnValue: Boolean); virtual; abstract;
```

This will define a global variable in the evaluator. There is currently no way to undefine a variable, but redefining it will replace the old value. The `VarValue` must live as long the evaluator does since there is no subscription to this object. If the `VarValue` is `**nil**`, a `VariableType` must be supplied, otherwise the `VariableType` can be `**nil**`.

### Evaluate

```delphi
procedure Evaluate(Ocl: string; Root: TBoldElement; Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil); virtual; abstract;
```

Evaluate will evaluate the expression OCL relative to `Root`. If `ResultElement` is not `**nil**` it will be set to the result when evaluating the expression. If the result of evaluating the expression is an existing [TBoldElement](TBoldElement.md), `ResultElement` will just refer to it, and `ResultElement`.[OwnsValue](TBoldIndirectElement.md#ownsvalue) will be `False`. Otherwise the result will be owned by `ResultElement`.

If `Subscriber` is not `**nil**` then `Subscribe` will get the default set of subscriptions on the result. If `ResubscribeAll` is `False`, subscriptions will be placed in the following way:

- Any change such that re-evaluating the expression would give a new value will give a notification to `Subscriber` with `RequestedEvent = breReEvaluate`.
- Any change such that re-evaluating the expression would result in new subscriptions being placed will give a notification with `RequestedEvent = breReSubscribe`.

If `ReSubscribeAll` is `True`, a `breReSubscribe` will always be sent.

### ExpressionType

```delphi
function ExpressionType(const Ocl: string; Context: TBoldElementTypeInfo; ReRaise: Boolean; const VariableList: TBoldExternalVariableList = nil): TBoldElementTypeInfo; virtual; abstract;
```

`ExpressionType` will return the type of the result that would be obtained if [Evaluate](TBoldEvaluator.md#evaluate) is called with a `Root` of type `Context`. If the expression is an invalid expression relative to the context, and `ReRaise` is `true`, an exception will be raised with an explanation of what went wrong in the syntactic or semantic parse. If `ReRaise` is `false`, the result will be set to `**nil**`.

### SetLookupOclDefinition

```delphi
procedure SetLookupOclDefinition(value: TBoldLookUpOclDefinition); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
