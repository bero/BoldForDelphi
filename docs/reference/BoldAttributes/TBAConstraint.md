# TBAConstraint

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAConstraint = class(TBABoolean)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](../BoldElements/TBoldElement.md)
6. [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md)
7. [TBoldMember](../BoldSystem/TBoldMember.md)
8. [TBoldAttribute](../BoldSystem/TBoldAttribute.md)
9. [TBAValueSet](TBAValueSet.md)
10. [TBABoolean](TBABoolean.md)
11. TBAConstraint

## Description

Constraints that are specified in the model can be evaluated and the result is an instance of TBAConstraint. This class inherits from TBABoolean, so it will have a truth-value corresponding to whether the constrain holds or is broken.

To indicate to a user what constraint is actually broken, each constraint has a number of string reperesentations:

- 1: (brDefault) N/Y
- 2: False/True
- 3: F/T
- 10: the model name of the constraint
- 11: the description of the constraint
- 12: the expression of the constraint
- 13: the [DisplayName](../BoldDomainElement/TBoldDomainElement.md#displayname) of the element that the constraint belongs to
- 14: the string representation of the element that the constraint belongs to

Given an object that has constraints, the following ocl expressions are useful:

- `anObject.constraints`: all constraints of the object
- `anObject.constraints->select(c|**not** c)`: all broken constraints of the object
- `anObject.constraints->select(c|**not** c)->isEmpty`: true if the object has no broken constraints

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Constraint](#constraint) |  | read-only |
| [OwningElement](#owningelement) | The element that has the constraint | read-only |

### Constraint

```delphi
property Constraint: TBoldConstraintRTInfo;
```

### OwningElement

```delphi
property OwningElement: TBoldElement;
```

Normally this will be a[TBoldObject](../BoldSystem/TBoldObject.md), but could be any element that can have constraints.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [DefaultSubscribe](#defaultsubscribe) |  | override |
| [Destroy](#destroy) |  | override |
| [GetContentAsInteger](#getcontentasinteger) |  | protected, override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [Initialize](#initialize) |  |  |
| [InitializeMember](#initializemember) |  | protected, override |
| [SubscribeToStringRepresentation](#subscribetostringrepresentation) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

### GetContentAsInteger

```delphi
function GetContentAsInteger: Integer; override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### Initialize

```delphi
procedure Initialize(Constraint: TBoldConstraintRTInfo; OwningElement: TBoldElement);
```

### InitializeMember

```delphi
procedure InitializeMember(OwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

### SubscribeToStringRepresentation

```delphi
procedure SubscribeToStringRepresentation(Representation: TBoldRepresentation; Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
