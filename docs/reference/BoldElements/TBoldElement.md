# TBoldElement

Superclass for all elements in Bold

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldElement = class(TBoldSubscribableObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. TBoldElement
6. **Direct subclasses**
7. [TBAValueSetValue](../BoldAttributes/TBAValueSetValue.md)
8. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
9. [TBoldMetaElement](TBoldMetaElement.md)

## Description

`TBoldElement` is the superclass for all elements in Bold that actually exist, or to be more precise that in themselves represent a value, and object or a piece of meta-information.

It also holds some high-level operations that apply to all Elements such as `Assign` and `Compare`.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsString](#asstring) | The default stringrepresentation of a TBoldElement |  |
| [BoldType](#boldtype) | The type of the element | read-only |
| [Evaluator](#evaluator) | Used for evaluating expressions relative to the expression | read-only |
| [ModifiedValueHolder](#modifiedvalueholder) | GUI locking mechanism of Bold | read-only |
| [Mutable](#mutable) | If the element can change | read-only |
| [StringRepresentation](#stringrepresentation) | A string representing the value of the element |  |

### AsString

```delphi
property AsString: string;
```

`AsString` is the default stringrepresentation of a TBoldElement. It is equivalent to [StringRepresentation](TBoldElement.md#stringrepresentation)[brDefault].

### BoldType

```delphi
property BoldType: TBoldElementTypeInfo;
```

All Bold elements refer to an object representing its type. The fact that everything has type information available in run-time is one of the key features of Bold for Delphi/Bold for C++.

### Evaluator

```delphi
property Evaluator: TBoldEvaluator;
```

Each `TBoldElement` has an evaluator that is used for evaluating expressions relative to the expression. The evaluator will actually be part of the runtime information for a [TBoldSystem](../BoldSystem/TBoldSystem.md), and the various subclasses to `TBoldElement` will pick it up in different ways.

### ModifiedValueHolder

```delphi
property ModifiedValueHolder: TObject;
```

This property is used by the GUI locking mechanism of Bold for Delphi/Bold for C++. It can be set using the methods [RegisterModifiedValueHolder](TBoldElement.md#registermodifiedvalueholder) and [UnRegisterModifiedValueHolder](TBoldElement.md#unregistermodifiedvalueholder). The locking mechanism is not mandatory, but should be handled by each component and ControlPack.

**See Also**

- [ObserverMayModify](TBoldElement.md#observermaymodify)

### Mutable

```delphi
property Mutable: Boolean;
```

An element that is mutable is an element that can be changed. Examples of mutable elements are the attributes of an object.

An immutable element can not be changed, ever. The instances of subclasses to [TBoldElementTypeInfo](TBoldElementTypeInfo.md) are immutable. The metainformation can not change when the application is running. The results of ocl evaluations are also immutable if they are not simple values. You are not allowed to change the result of '`aPerson.firstName + aPerson.lastName`'.

One consequence of being immutable is that subscriptions to value changes will be disregarded, another that Bold at its discretion may copy/cache the value for efficiency reasons.

A element may be explicitly made immutable by calling [MakeImmutable](TBoldElement.md#makeimmutable). This can be done e.g. to enforce security restrictions.

There is no way to make an immutable value mutable again.

### StringRepresentation

```delphi
property StringRepresentation[Representation:TBoldRepresentation]: string;
```

All Bold elements may represent themselves as strings. The string should in some way reflect the value of the element. The `Representation` can be used to implement variations, such as brief or verbose.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) | Copy the value of one element into another | virtual |
| [AssignError](#assignerror) | Signal an error in an assign operation | protected |
| [CloneIfPossible](#cloneifpossible) | If the element can be cloned, then it will be cloned and the clone will be returned. | protected, virtual |
| [CompareError](#compareerror) | Signal an error in a compare operation | protected |
| [CompareTo](#compareto) | Compare two elements |  |
| [CompareToAs](#comparetoas) | Compare two elements | virtual |
| [CompareTypeError](#comparetypeerror) | Signal a CompareType error in a compare operation | protected |
| [DefaultSubscribe](#defaultsubscribe) | Place default subscriptions | abstract |
| [Destroy](#destroy) | Destructor | override |
| [EnsureValidString](#ensurevalidstring) | Raise an exception if the string is not valid. |  |
| [EvaluateAndSubscribeToExpression](#evaluateandsubscribetoexpression) | Evaluate the expression, and place subscriptions to notify when the value of the expression has changed. |  |
| [EvaluateExpression](#evaluateexpression) | Evaluate the expression. |  |
| [EvaluateExpressionAsDirectElement](#evaluateexpressionasdirectelement) | As EvaluateExpression, but sends the resulting value as the return value of function. |  |
| [EvaluateExpressionAsNewElement](#evaluateexpressionasnewelement) | As EvaluateExpression, but returns a newly created element. |  |
| [EvaluateExpressionAsString](#evaluateexpressionasstring) | As EvaluateExpression, but sends the string representation of the expression's result as the return value of function. |  |
| [GetAsList](#getaslist) | Gives the element as a list. | abstract |
| [GetAsValue](#getasvalue) | Makes itself the value of an indirect element. | virtual |
| [GetAsVariant](#getasvariant) | The value of the element as a variant | virtual |
| [GetBoldType](#getboldtype) | Get-method for the BoldType property | protected, abstract |
| [GetEvaluator](#getevaluator) | Get-method for the Evaluator property | protected, virtual |
| [GetStringRepresentation](#getstringrepresentation) | Get-method for the StringRepresentation property | protected, virtual |
| [IsEqual](#isequal) | True if the element is concidered equal to BoldElement |  |
| [IsEqualAs](#isequalas) | True if the element is equal to BoldElement according to the CompareType. | virtual |
| [MakeImmutable](#makeimmutable) | Sets the element to immutable. |  |
| [MutableError](#mutableerror) | Signal an error because the element is immutable | protected |
| [ObserverMayModify](#observermaymodify) | Bold-internal | virtual |
| [ObserverMayModifyAsString](#observermaymodifyasstring) | Bold-internal | virtual |
| [PrepareToDestroy](#preparetodestroy) | Bold-internal |  |
| [RegisterModifiedValueHolder](#registermodifiedvalueholder) | Sets the ModifiedValueHolder to the observer object. |  |
| [SetAsVariant](#setasvariant) | Set the value of the element from a variant. | virtual |
| [SetStringRepresentation](#setstringrepresentation) | Set-method for the StringRepresentation property. | protected, virtual |
| [SubscribeToExpression](#subscribetoexpression) | Places the subscriptions necessary to detect when the result of Expression changes |  |
| [SubscribeToStringRepresentation](#subscribetostringrepresentation) | Place subscriptions to be notified when the string representation of the element changes. | virtual |
| [UnRegisterModifiedValueHolder](#unregistermodifiedvalueholder) | Remove the observer object as the ModifiedValueHolder |  |
| [ValidateCharacter](#validatecharacter) | Return true if C is a valid character in the string representation Representation of the element. | virtual |
| [ValidateString](#validatestring) | Return true if Value is a valid string representation of the element. | virtual |

### Assign

```delphi
procedure Assign(Source: TBoldElement); virtual;
```

The value of the source element will be copied into the element, provided the elements are assignment compatible, i.e. of sufficiently similar types.

### AssignError

```delphi
procedure AssignError(BoldElement: TBoldElement);
```

This method should only be called by the implementation of [Assign](TBoldElement.md#assign) in the subclasses of `TBoldElement` when the type of the source element is not compatible.

### CloneIfPossible

```delphi
function CloneIfPossible: TBoldElement; virtual;
```

If the element can be cloned, then it will be cloned and the clone will be returned. Otherwise a `**nil**`-pointer will be returned.

In general, only members can be cloned, but this method could be overriden by a businessobject to clone itself.

### CompareError

```delphi
procedure CompareError(BoldElement: TBoldElement);
```

This method should only be called by the implementation of [CompareToAs](TBoldElement.md#comparetoas) in the subclasses of `TBoldElement` when the types of the elements are not compatible.

### CompareTo

```delphi
function CompareTo(BoldElement: TBoldElement): Integer;
```

Compare two elements. `CompareTo` will return 0 if the two elements are equal, -1 if the argument is before self and 1 if self is before the argument. This is equivalent to calling [CompareToAs](TBoldElement.md#comparetoas) with `ctDefault` as `CompareType`.

### CompareToAs

```delphi
function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; virtual;
```

Compare two elements. `CompareToAs` will return 0 if the two elements are equal, -1 if the argument is before self and 1 if self is before the argument.

### CompareTypeError

```delphi
procedure CompareTypeError(CompType: TBoldCompareType; BoldElement: TBoldElement);
```

Called by an implementation of [CompareToAs](TBoldElement.md#comparetoas) if the elements do not support the `CompareType`.

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); virtual; abstract;
```

Will place the most common subscriptions on the element.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### EnsureValidString

```delphi
procedure EnsureValidString(const Value: string; Representation: TBoldRepresentation);
```

Will raise an exception if the string `Value` is not valid for the element, per the [ValidateString](TBoldElement.md#validatestring) method.

### EvaluateAndSubscribeToExpression

```delphi
procedure EvaluateAndSubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; resultElement: TBoldIndirectElement; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
```

Evaluate the expression, and place subscriptions to notify when the value of the expression has changed. The element is used as root of the expression. resultElement is an indirect element that should be provided by the caller. Its value will be the result of the expression. Subscriber is the subscriber that will receive the subscriptions. If it is `**nil**` then no subscriptions will be placed. If `Resubscribe` is `true`, then all subscriptons will be placed with `breResubscribe` as `RequestedEvent`.

If `EvaluateInPS` is `true`, then the evaluator will attempt to pass on the expression to the persistence mechanism. If not, the expression will be evaluated in memory. The `VariableList` can be used to supply variable definitions to the expression.

**See Also**

- [TBoldIndirectElement](TBoldIndirectElement.md)
- About Ocl2Sql

### EvaluateExpression

```delphi
procedure EvaluateExpression(const Expression: TBoldExpression; resultElement: TBoldIndirectElement; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
```

Evaluate the expression. The element is used as root of the expression. resultElement is an indirect element that should be provided by the caller. Its value will be the result of the expression.

If `EvaluateInPS` is `true`, then the evaluator will attempt to pass on the expression to the persistence mechanism. If not, the expression will be evaluated in memory. The `VariableList` can be used to supply variable definitions to the expression.

**See Also**

- [TBoldIndirectElement](TBoldIndirectElement.md)
- About Ocl2Sql

### EvaluateExpressionAsDirectElement

```delphi
function EvaluateExpressionAsDirectElement(const Expression: TBoldExpression; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
```

This method works the same way as [EvaluateExpression](TBoldElement.md#evaluateexpression), with the difference that it returns the resulting element rather than forcing the caller to supply an indirect element to hold the result. This, however, means that the expression must result in an element that is owned (transitively) by a system, i.e. its result must be a Bold object, an attribute or role of a Bold object, an "all instances"-list, a Bold type info or the system itself. Expressions that would not result in an owned element are, for instance, the sum of two attributes, or filtered or sorted lists. Such expressions will return `**nil**`.

### EvaluateExpressionAsNewElement

```delphi
function EvaluateExpressionAsNewElement(const Expression: TBoldExpression; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): TBoldElement;
```

The result of this function is always an element that must be destroyed after use. If the expression results in an attribute of an object, it will first be cloned before it is returned to you.

### EvaluateExpressionAsString

```delphi
function EvaluateExpressionAsString(const Expression: TBoldExpression; Representation: TBoldRepresentation; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil): string;
```

This method works the same way as [EvaluateExpression](TBoldElement.md#evaluateexpression), but sends the string representation of the `expression`'s result as the return value of function.

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); virtual; abstract;
```

Different elements will have different ways of converting themselves to lists. Elements that are already lists, such as [TBoldObjectList](../BoldSystem/TBoldObjectList.md) or [TBoldMemberList](../BoldSystem/TBoldMemberList.md), will yield themselves as result. Elements that represent more basic entities, such as [TBoldObject](../BoldSystem/TBoldObject.md) or [TBoldAttribute](../BoldSystem/TBoldAttribute.md), will yeild a list with one element. The result will be returned as the `Value` of the `ResultList` indirect element.

### GetAsValue

```delphi
procedure GetAsValue(resultElement: TBoldIndirectElement); virtual;
```

Makes itself the value of an indirect element.

### GetAsVariant

```delphi
function GetAsVariant: Variant; virtual;
```

This function returns the value of the element as a variant.

### GetBoldType

```delphi
function GetBoldType: TBoldElementTypeInfo; virtual; abstract;
```

Get-method for the BoldType property

### GetEvaluator

```delphi
function GetEvaluator: TBoldEvaluator; virtual;
```

Get-method for the Evaluator property

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; virtual;
```

Get-method for the [StringRepresentation](TBoldElement.md#stringrepresentation) property. This method can be overridden on [TBoldObject](../BoldSystem/TBoldObject.md) subclasses (i.e. the modelled business classes for a project), or [TBoldAttribute](../BoldSystem/TBoldAttribute.md) subclasses, thus providing customized string represenations.

### IsEqual

```delphi
function IsEqual(BoldElement: TBoldElement): Boolean;
```

The behaviour of this function can be modified by overriding [IsEqualAs](TBoldElement.md#isequalas).

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; virtual;
```

`True` if the element is equal to `BoldElement` according to the `CompareType`. Elements can be compared if they are of sufficiently similar types. This method can be overridden to implement custom criteria for equality.

### MakeImmutable

```delphi
procedure MakeImmutable;
```

Sets the element to immutable. See [Mutable](TBoldElement.md#mutable).

### MutableError

```delphi
procedure MutableError(const NewValue: string);
```

Called by subclasses to `TBoldElement` if an error occured because the element is immutable.

### ObserverMayModify

```delphi
function ObserverMayModify(Observer: TObject): Boolean; virtual;
```

Bold-internal

### ObserverMayModifyAsString

```delphi
function ObserverMayModifyAsString(Representation: TBoldRepresentation; observer: TBoldSubscriber): Boolean; virtual;
```

Bold-internal

### PrepareToDestroy

```delphi
procedure PrepareToDestroy;
```

This method is intended to be called early in each destructor in the subclasses of `TBoldElement`. It will call FreePublisher, which in turn will notify all subscribers of the elements' imminent destruction.

**Implementation notes**  
The method is virtual, so subclasses may override it to perform further actions that need to be performed prior to destroying the element. It is not intended to be overridden by developers using Bold.

### RegisterModifiedValueHolder

```delphi
procedure RegisterModifiedValueHolder(observer: TObject);
```

Sets the `ModifiedValueHolder` to the observer object.

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); virtual;
```

Sets the value of the element from a variant. This method should be overridden by custom attribute classes (i.e. descendents to [TBoldAttribute](../BoldSystem/TBoldAttribute.md)).

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); virtual;
```

Set-method for the `StringRepresentation` property. This method can be overridden in the subclasses to implement customized string representations.

### SubscribeToExpression

```delphi
procedure SubscribeToExpression(const Expression: TBoldExpression; Subscriber: TBoldSubscriber; Resubscribe: Boolean = false; EvaluateInPS: Boolean = false; const VariableList: TBoldExternalVariableList = nil);
```

Places the subscriptions necessary to detect when the result of Expression changes. See [EvaluateAndSubscribeToExpression](TBoldElement.md#evaluateandsubscribetoexpression) for details.

### SubscribeToStringRepresentation

```delphi
procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); virtual;
```

Place subscriptions to be notified when the string representation of the element changes. This method should be overridden in subclasses that implement custom string representations.

**See Also**

- [GetStringRepresentation](TBoldElement.md#getstringrepresentation)
- [SetStringRepresentation](TBoldElement.md#setstringrepresentation)

### UnRegisterModifiedValueHolder

```delphi
procedure UnRegisterModifiedValueHolder(observer: TObject);
```

Remove the observer object as the `ModifiedValueHolder`

### ValidateCharacter

```delphi
function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; virtual;
```

Return `true` if `C` is a valid character in the string representation `Representation` of the element. Override this method to provide quick validation of user input.

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; virtual;
```

Return `true` if `Value` is a valid string representation of the element, using the representation `Representation`. Override this method to provide validation of user input.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
