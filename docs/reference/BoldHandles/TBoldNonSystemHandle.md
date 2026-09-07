# TBoldNonSystemHandle

This class introduces the StaticSystemHandle property. It is an abstract class.

**Unit**: [BoldHandles](index.md)

## Declaration

```delphi
TBoldNonSystemHandle = class(TBoldElementHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](TBoldElementHandle.md)
4. TBoldNonSystemHandle
5. **Direct subclasses**
6. `TBoldReferenceHandle`
7. [TBoldRootedHandle](../BoldRootedHandles/TBoldRootedHandle.md)
8. `TBoldSQLHandle`
9. `TBoldVariableHandle`

## Description

This class introduces the [StaticSystemHandle](TBoldNonSystemHandle.md#staticsystemhandle) property. It is an abstract class.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [StaticSystemHandle](#staticsystemhandle) | SystemHandle used if no Value is available |  |

### StaticSystemHandle

```delphi
property StaticSystemHandle: TBoldAbstractSystemHandle;
```

Setting this handle will affect the way [StaticSystemTypeInfo](TBoldElementHandle.md#staticsystemtypeinfo) and thus ultimately [StaticBoldType](TBoldElementHandle.md#staticboldtype) are calculated. Its use is described in the overview of the class for each handle. If the handle is not connected (recursively) to a system handle through some other property, this property should be set to point to the SystemHandle to which the handle "belongs".

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Constructor | override |
| [Destroy](#destroy) | Destructor | override |
| [GetStaticSystemTypeInfo](#getstaticsystemtypeinfo) | Overrides TBoldElementHandle.GetStaticSystemTypeInfo | protected, override |
| [SetStaticSystemHandle](#setstaticsystemhandle) | Set-method for the StaticSystemHandle property | protected, virtual |

### Create

```delphi
constructor Create(Owner: TComponent); override;
```

Constructor. `TBoldNonSystemHandle` is an abstract class, and thus not intended to be instansiated.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor

### GetStaticSystemTypeInfo

```delphi
function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override; See also Ancestor Method
```

Overrides [GetStaticSystemTypeInfo](TBoldElementHandle.md#getstaticsystemtypeinfo)

### SetStaticSystemHandle

```delphi
procedure SetStaticSystemHandle(Value: TBoldAbstractSystemHandle); virtual;
```

Set-method for the StaticSystemHandle property

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
