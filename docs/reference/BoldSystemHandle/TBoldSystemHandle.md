# TBoldSystemHandle

A TBoldSystemHandle component is used in the IDE of Delphi to represent an entire system of business elements, also known as an object-space.

**Unit**: [BoldSystemHandle](index.md)

## Declaration

```delphi
TBoldSystemHandle = class(TBoldAbstractSystemHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
4. [TBoldAbstractSystemHandle](../BoldHandles/TBoldAbstractSystemHandle.md)
5. TBoldSystemHandle

## Description

In order to use it you must set [SystemTypeInfoHandle](../BoldHandles/TBoldAbstractSystemHandle.md#systemtypeinfohandle) to point at a [TBoldSystemTypeInfoHandle](../BoldHandles/TBoldSystemTypeInfoHandle.md) which in turn must be connected to the `TBoldModel` for the system. If the system is to be persistent [PersistenceHandle](TBoldSystemHandle.md#persistencehandle) must also be connected. The [active](../BoldHandles/TBoldAbstractSystemHandle.md#active) property decides whether the system is active or not. In most cases `Active` is left `false` at designtime, and [AutoActivate](TBoldSystemHandle.md#autoactivate) is used instead.

**Value**  
The [value](../BoldHandles/TBoldElementHandle.md#value) of the handle is the system owned by the handle. It will be nil if [active](../BoldHandles/TBoldAbstractSystemHandle.md#active) is false.

**Typing**  
The [StaticBoldType](../BoldHandles/TBoldElementHandle.md#staticboldtype) is the type information for the entire system, which is held by [SystemTypeInfoHandle](../BoldHandles/TBoldAbstractSystemHandle.md#systemtypeinfohandle).

**Bold Events**  
`TBoldSystemHandle` is a subclass of [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md), and can therefore by subscribed to using `AddSmallSubscription`. A `TBoldSystemHandle` can send the following events:

- `beDestroying`: Sent when the handle is about to be destroyed.
- `beValueIdentityChanged`: Sent whenever `Active` or `TBoldAbstractSystemHandle.SystemTypeInfoHandle` is changed.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AutoActivate](#autoactivate) | Indicates if the system should be autmatically started when the application starts. |  |
| [OnPreUpdate](#onpreupdate) |  |  |
| [PersistenceHandle](#persistencehandle) | If a system is to be persistent it must be connected to a TBoldPersistenceHandle . |  |
| [Persistent](#persistent) | This read-only property is true if PersistenceHandle is assigned. | read-only |
| [RegionFactory](#regionfactory) |  | read-only |

### AutoActivate

```delphi
property AutoActivate: Boolean;
```

If `AutoActivate` is `True`, [Active](../BoldHandles/TBoldAbstractSystemHandle.md#active) will be set to `True` at runtime, otherwise `Active` will be `False` at start-up, and must be set to `True` programatically.

### OnPreUpdate

```delphi
property OnPreUpdate: TNotifyEvent;
```

### PersistenceHandle

```delphi
property PersistenceHandle: TBoldPersistenceHandle;
```

The persistence-handle defines all properties related to persistence. Persistence will only work properly if the `PersistenceHandle` is connected to the same model as the system-handle (or at least one containing an identical model).

**Advanced information**  
The `PersistenceHandle` is stateless, so it is possible to connect multiple system-handles to the same persistence-handle.

### Persistent

```delphi
property Persistent: Boolean;
```

`Persistent` evaluates to `True` if the [PersistenceHandle](TBoldSystemHandle.md#persistencehandle) is assigned.

### RegionFactory

```delphi
property RegionFactory: TBoldRegionFactory;
```

Creates regions for the system, according to the region definitions supplied in the model.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [create](#create) | Creates an instance of TBoldSystemHandle | override |
| [DefineProperties](#defineproperties) |  | protected, override |
| [Destroy](#destroy) |  | override |
| [GetActive](#getactive) |  | protected, override |
| [GetSystem](#getsystem) |  | protected, override |
| [GetValue](#getvalue) |  | protected, override |
| [InstallOclDefinitionLookUp](#installocldefinitionlookup) | Used by a repository to provide a Lookup-function for the OCL evaluator |  |
| [Loaded](#loaded) |  | protected, override |
| [ModelChanged](#modelchanged) |  | protected, override |
| [RefersToComponent](#referstocomponent) | Determine if there is a relationship to the component passed as parameter. | override |
| [SetActive](#setactive) |  | protected, override |
| [UpdateDatabase](#updatedatabase) | Calling UpdateDatabase will update the database with all changes made in the objectspace belonging to the component. |  |

### create

```delphi
constructor create(owner: TComponent); override; See also Ancestor Method
```

Use create to create an instance of `TBoldSystemHandle`.

Note that it needs some information to do its thing, most notably the SystemTypeInfoHandle must be set. To be able to persist the object space, the PersistenceHandle needs to be set.

### DefineProperties

```delphi
procedure DefineProperties(Filer: TFiler); override;
```

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

Destroys the `TBoldSystemHandle`, freeing all resources, including the entire [TBoldSystem](../BoldSystem/TBoldSystem.md) if it is still active.

**Bold events**  
Calling `Destroy` can result in the following events:

- `beDestroying`: Sent when the handle is about to be destroyed, i.e. before any part of the destruction has been performed.

Don't call `Destroy` directly. Use `Free` instead.

### GetActive

```delphi
function GetActive: Boolean; override; See also Ancestor Method
```

### GetSystem

```delphi
function GetSystem: TBoldSystem; override; See also Ancestor Method
```

### GetValue

```delphi
function GetValue: TBoldElement; override; See also Ancestor Method
```

### InstallOclDefinitionLookUp

```delphi
procedure InstallOclDefinitionLookUp(const Value: TBoldLookUpOclDefinition);
```

This method is intended to be used by the `TBoldOCLRepository` to provide the OCL evaluator with a callback to perform a lookup of an expression in the repository

### Loaded

```delphi
procedure Loaded; override;
```

### ModelChanged

```delphi
procedure ModelChanged; override; See also Ancestor Method
```

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

`RefersToComponent` determins if there is a relationship between the `TBoldSystemHandle` and the component passed as parameter. It is used internally to avoid circular dependencies between components.

### SetActive

```delphi
procedure SetActive(Value: Boolean); override; See also Ancestor Method
```

### UpdateDatabase

```delphi
procedure UpdateDatabase;
```

It is equivalent to calling [BoldSystem.UpdateDatabase](../BoldSystem/TBoldSystem.md#updatedatabase).

If there is no system, most likely because the [Active](../BoldHandles/TBoldAbstractSystemHandle.md#active) property is `False`, invoking this method will raise an exception.

## Events

| Name | Summary | Notes |
|---|---|---|
| [OnOptimisticLockingFailed](#onoptimisticlockingfailed) | Event that will be called if optimistic locking fails |  |

### OnOptimisticLockingFailed

```delphi
TBoldOptimisticLockingFailedEvent = procedure(UpdateList, FailureList: TBoldObjectList; const FailureReason: String) of object;
```

If the application has optimistic locking activated, it might detect a conflict during an update. If this happens this event will be called with the objects that failed optimistic locking.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
