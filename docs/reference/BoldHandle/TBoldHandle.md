# TBoldHandle

Abstract superclass for wrapping making clases IDE-manipulatable.

**Unit**: [BoldHandle](index.md)

## Declaration

```delphi
TBoldHandle = class(TBoldSubscribableComponent)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. TBoldHandle
4. **Direct subclasses**
5. `TBoldAbstractObjectUpgraderHandle`
6. `TBoldClientHandle`
7. `TBoldListenerHandle`
8. `TBoldOLLEHandle`
9. `TBoldPersistenceHandle`
10. `TBoldServerHandle`
11. `TBoldUMLModelLink`

## Description

The Delphi/BCB/Kylix IDE allows efficent designtime manipulation of classes decending from TComponent. Subclassing TBoldHandle way for providing access from the IDE to classes that don't decend from TComponent. Since subscriptions are used extensivley in Bold THandle inherits from [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md). A TBoldHandle is simply a component holding a reference to a handled object. Handles are used extensively in Bold to provide placeable IDE access to various parts of Bold.

| **Note** |
|---|

| A somewhat confusing fact is that not all placeable nonvisual components in Bold descend from `TBoldHandle`. This is particularly true of [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md) and its subclasses. |
|---|

## Properties

| Name | Summary | Notes |
|---|---|---|
| [HandledObject](#handledobject) |  | read-only |

### HandledObject

```delphi
property HandledObject: TObject;
```

This is the object handled by the handle. It will normally be allocated eith by the constructor of the concrete subclass, or by the overriden [GetHandledObject](TBoldHandle.md#gethandledobject) in the case of lazy creation.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Constructor. | override |
| [Destroy](#destroy) | Destructor. | override |
| [GetHandledObject](#gethandledobject) | Get method for HandledObject | protected, abstract |

### Create

```delphi
constructor Create(Owner: TComponent); override;
```

Constructor.

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destructor. The destructor will call [FreePublisher](../BoldSubscription/TBoldSubscribableComponent.md#freepublisher) early in the process.

### GetHandledObject

```delphi
function GetHandledObject: TObject; virtual; abstract;
```

This method is abstract virtual, and must be overridden in all subclasses.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
