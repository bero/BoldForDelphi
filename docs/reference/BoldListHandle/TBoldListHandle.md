# TBoldListHandle

TBoldListHandle is used to get a list from a root using an OCL expression

**Unit**: [BoldListHandle](index.md)

## Declaration

```delphi
TBoldListHandle = class(TBoldCursorHandle, IBoldOCLComponent)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
4. [TBoldNonSystemHandle](../BoldHandles/TBoldNonSystemHandle.md)
5. [TBoldRootedHandle](../BoldRootedHandles/TBoldRootedHandle.md)
6. [TBoldAbstractListHandle](../BoldAbstractListHandle/TBoldAbstractListHandle.md)
7. `TBoldCursorHandle`
8. TBoldListHandle

## Description

A `TBoldListHandle` is used to represent a list obtained by applying an OCL expression relative to the value of a [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md) that is set using the [RootHandle](../BoldRootedHandles/TBoldRootedHandle.md#roothandle).

A handle can keep it's value current by using the subscription mechanism in Bold. This behavior is controlled by the Subscribe property.

The list is obtained by calling [GetAsList](../BoldElements/TBoldElement.md#getaslist) on the value obtained by evaluating [Expression](TBoldListHandle.md#expression). This means that if the result of the evaluation is an individual element rather than a List, a list of one element will be created.

The list may be further modified by a filtering and/or sorting. This is done by setting the [BoldFilter](TBoldListHandle.md#boldfilter) and/or [BoldComparer](TBoldListHandle.md#boldcomparer) properties.

The list will in most cases be a list of Business Objects (`TBoldObject`), but can be a list of any [TBoldElement](../BoldElements/TBoldElement.md), e.g. a list of strings (`TBAString`).

In addition to keeping a list, a `TBoldListHandle` also holds a current position in the list. The `Value` property will be this element, rather that the whole list. The list itself can be accessed through the [list](../BoldAbstractListHandle/TBoldAbstractListHandle.md#list) property.

**Usage**

The most common use of the component is placing it on a form or datamodule, connecting its `RootHandle` to another handle, and setting the `expression`.

It can now be used by connecting Bold-aware controls to it. If it is connected to a multi-valued control, such as a `TBoldGrid`, the elements of its `List` will be displayed. If it is connected to a single-valued control, such as a `TBoldEdit`, `Value`, i.e. `CurrentElement`, will be displayed in the control.

It can also be used by other handles. In this case they will be evaluated relative to `Value`.

**Implementation notes**

Like all handles, `TBoldListHandle` acts as an adapter between the business object/server world and the IDE/Client world.

`TBoldListHandle` has a rather complex internal structure. Depending on the properties it will internally aggregate other handles to perform OCL-evaluation, sorting and filtering.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [BoldComparer](#boldcomparer) | Optional reordering/sorting component |  |
| [BoldFilter](#boldfilter) | Optional filter component |  |
| [EvaluateInPS](#evaluateinps) | If Expression should be evaluated in the database, or in memory. |  |
| [Expression](#expression) | OCL expression to evaluate the resulting list element |  |
| [MutableListExpression](#mutablelistexpression) | List expression yielding mutable list |  |
| [Variables](#variables) | Allows the expression to have more than one context |  |

### BoldComparer

```delphi
property BoldComparer: TBoldComparer;
```

Setting the BoldComparer property will use the comparer to sort the list produced by Expression.

| **Note** |
|---|

| Note that a sorted list is regarded as derived and may therefore not be modified. However, the `MutableList` property will understand the concept of comparers, and can be used for some list-manipulations |
|---|

| **Note** |
|---|

| In most cases sorting can be done directly in `Expression`, by using BoldSoft's extensions to OCL. This often more efficient convenient, and should be done whenever possible. |
|---|

### BoldFilter

```delphi
property BoldFilter: TBoldFilter;
```

Setting the `BoldFilter` property will apply the filter to the list produced by Expression.

| **Note** |
|---|

| A filtered list is regarded as derived and may therefore not be modified. |
|---|

| **Note** |
|---|

| In most cases filtering can be done directly in `Expression`. This is often far more convenient, and should be done whenever possible. |
|---|

### EvaluateInPS

```delphi
property EvaluateInPS: Boolean;
```

Normally Ocl expressions are evaluated in memory, but this can lead to great inefficiencies when numerous objects have to be loaded. If this property is set to true an attempt to execute the expression directly in the database will be made. If this fails, there will be an exception.

| **Note** |
|---|

| If this property is true, the component will ignore the setting of [Subscribe](../BoldRootedHandles/TBoldRootedHandle.md#subscribe). |
|---|

For further information, see Ocl2Sql

### Expression

```delphi
property Expression: TBoldExpression;
```

Expression is a string containing an OCL expression. The [list](../BoldAbstractListHandle/TBoldAbstractListHandle.md#list) of the handle is defined by evaluating `Expression` with `BoldHandle.Value` as the context, and then calling `AsList`.

At design-time the property editor provides support in writing the expression.

### MutableListExpression

```delphi
property MutableListExpression: TBoldExpression;
```

The list as described by the [Expression](TBoldListHandle.md#expression) may be immutable, i.e. not possible to change. This is the case of any any kind of derived lists; `Person.allInstances->orderBy(firstName)`.

If a `TBoldNavigator` is connected to the list handle, it will not be possible to add objects to the list by pressing the [+] button, nor delete objects by pressing the [-] button. To overcome this, specify an expression yielding a mutable list; `Person.allInstances`. This expression will be used if the list produced by the `Expression`-property is immutable.

The resulting mutable list is accessible via the MutableList property.

### Variables

```delphi
property Variables: TBoldOclVariables;
```

If the OCL expression requires more than one context, additional context must be imported using a `TBoldOclVariables` component

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Creates an instance of TBoldListHandle | override |
| [Destroy](#destroy) | Destroys an instance of TBoldListHandle | override |
| [GetMutableList](#getmutablelist) | Overrides TBoldAbstractListHandle.GetMutableList | protected, override |
| [GetRootHandle](#getroothandle) | Overrides TBoldRootedHandle.GetRootHandle | protected, override |
| [Loaded](#loaded) | Overrides TComponent.Loaded | protected, override |
| [MarkOutOfDate](#markoutofdate) | Forces a refresh of the value | override |
| [Notification](#notification) | Overrides TComponent.Notification | protected, override |
| [RefersToComponent](#referstocomponent) | Determines if this instance of TBoldListHandle in some way is referring to the component passed as parameter. | override |
| [SetRootHandle](#setroothandle) | Overrides TBoldRootedHandle.SetRootHandle | protected, override |
| [SetStaticSystemHandle](#setstaticsystemhandle) | Overrides TBoldNonSystemHandle.SetStaticSystemHandle | protected, override |
| [SetSubscribe](#setsubscribe) | Overrides TBoldRootedHandle.SetSubscribe | protected, override |

### Create

```delphi
constructor Create(Owner: TComponent); override; See also Ancestor Method
```

Use `Create` to create an instance of `TBoldListHandle.`

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Do not call `Destroy`, call `Free` instead.

### GetMutableList

```delphi
function GetMutableList: TBoldList; override; See also Ancestor Method
```

Overrides [GetMutableList](../BoldAbstractListHandle/TBoldAbstractListHandle.md#getmutablelist)

### GetRootHandle

```delphi
function GetRootHandle: TBoldElementHandle; override; See also Ancestor Method
```

Overrides [GetRootHandle](../BoldRootedHandles/TBoldRootedHandle.md#getroothandle)

### Loaded

```delphi
procedure Loaded; override; See also Ancestor Method
```

Overrides TComponent.Loaded

### MarkOutOfDate

```delphi
procedure MarkOutOfDate; override; See also Ancestor Method
```

Call `MarkOutOfDate` to force the component to refresh its value.

### Notification

```delphi
procedure Notification(AComponent: TComponent; Operation: TOperation); override;
```

Overrides TComponent.Notification

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

Determines if this instance of `TBoldListHandle` in some way is referring to the component passed as parameter.

### SetRootHandle

```delphi
procedure SetRootHandle(const Value: TBoldElementHandle); override; See also Ancestor Method
```

Overrides [SetRootHandle](../BoldRootedHandles/TBoldRootedHandle.md#setroothandle)

### SetStaticSystemHandle

```delphi
procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); override; See also Ancestor Method
```

Overrides [SetStaticSystemHandle](../BoldHandles/TBoldNonSystemHandle.md#setstaticsystemhandle)

### SetSubscribe

```delphi
procedure SetSubscribe(Value: Boolean); override; See also Ancestor Method
```

Overrides [SetSubscribe](../BoldRootedHandles/TBoldRootedHandle.md#setsubscribe)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
