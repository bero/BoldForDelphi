# TBoldMetaElement

The superclass of all elements holding information describing the Bold system

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldMetaElement = class(TBoldElement)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. [TBoldSubscribableObject](../BoldSubscription/TBoldSubscribableObject.md)
5. [TBoldElement](TBoldElement.md)
6. TBoldMetaElement
7. **Direct subclasses**
8. [TBoldElementTypeInfo](TBoldElementTypeInfo.md)
9. `TBoldMetaElementWithConstraint`

## Description

`TBoldMetaElement` is the superclass of all elements holding information describing the Bold system as such. Its sibling class [TBoldDomainElement](../BoldDomainElement/TBoldDomainElement.md) is the superclass for all classes representing the information held by the Bold system.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [DelphiName](#delphiname) | The DelphiName is the Delphi-identifier representing the meta element. | read-only |
| [ExpressionName](#expressionname) | The ExpressionName is the name used in ocl-expressions for the metaelement. | read-only |
| [ModelName](#modelname) | Name of the original model element | read-only |

### DelphiName

```delphi
property DelphiName: string;
```

The `DelphiName` is the Delphi-identifier representing the meta element.

### ExpressionName

```delphi
property ExpressionName: string;
```

The `ExpressionName` is the name used in ocl-expressions for the metaelement.

### ModelName

```delphi
property ModelName: string;
```

This is the name of the original model element.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Constructor |  |
| [DefaultSubscribe](#defaultsubscribe) | Overrides TBoldElement.DefaultSubscribe. | override |
| [GetAsList](#getaslist) | Overrides TBoldElement.GetAsList. | override |
| [GetStringRepresentation](#getstringrepresentation) | Overrides TBoldElement.GetStringRepresentation. | protected, override |
| [IsEqualAs](#isequalas) | Overrides TBoldElement.IsEqualAs. | override |

### Create

```delphi
constructor Create(const ModelName: string; const ExpressionName: string; const DelphiName: string);
```

Constructor

### DefaultSubscribe

```delphi
procedure DefaultSubscribe(Subscriber: TBoldSubscriber; RequestedEvent: TBoldEvent = breReEvaluate); override; See also Ancestor Method
```

Since MetaElements are always immutable, and are only destroyed when the system is destroyed, it is not possible to subscribe to meta elements.

### GetAsList

```delphi
procedure GetAsList(ResultList: TBoldIndirectElement); override; See also Ancestor Method
```

There is currently no subclass of [TBoldList](../BoldSystem/TBoldList.md) that supports meta-elements. Calling `GetAsList` on a meta element will raise an exception.

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

Overrides [GetStringRepresentation](TBoldElement.md#getstringrepresentation)

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override; See also Ancestor Method
```

Overrides [IsEqualAs](TBoldElement.md#isequalas)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
