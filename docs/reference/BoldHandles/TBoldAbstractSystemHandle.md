# TBoldAbstractSystemHandle

Internal.

**Unit**: [BoldHandles](index.md)

## Declaration

```delphi
TBoldAbstractSystemHandle = class(TBoldElementHandle)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](TBoldElementHandle.md)
4. TBoldAbstractSystemHandle
5. **Direct subclasses**
6. [TBoldSystemHandle](../BoldSystemHandle/TBoldSystemHandle.md)

## Description

This is a pure implementation class in order to provide internal separation in Bold. It is of no interest to normal developers. The interesting information is found in [TBoldSystemHandle](../BoldSystemHandle/TBoldSystemHandle.md).

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Active](#active) | Use to check or change the status of the SystemHandle |  |
| [IsDefault](#isdefault) | The system is the default system. |  |
| [System](#system) | If TBoldAbstractSystemHandle.Active is true, System is the objectspace owned by the handle. | read-only |
| [SystemTypeInfoHandle](#systemtypeinfohandle) | The handle to the system type information to be used by the system |  |

### Active

```delphi
property Active: Boolean;
```

This property decides if the objectspace owned by the handle is to be active or not. When `Active` is `True`, the [value](TBoldElementHandle.md#value) of the handle is a [TBoldSystem](../BoldSystem/TBoldSystem.md) as defined by the [SystemTypeInfoHandle](TBoldAbstractSystemHandle.md#systemtypeinfohandle) and [PersistenceHandle](../BoldSystemHandle/TBoldSystemHandle.md#persistencehandle) properties. When `Active` is `false`, `Value` is `**nil**`. The value is also available correctly typed as [System](TBoldAbstractSystemHandle.md#system).

Setting `Active` to `true` results in the creation of the objectspace, and activation of the persistence mechanism (e.g. database logon).

It is possible to set `Active` `true` at design-time. The utility of this varies depending on which features of Bold have been used. Basic types and OCL expressions always work at design time, while anything requiring user-written code such as members derived in code, methods on the domain objects and events on controls (particularly handles and renderers) work only at runtime.

The recommended practice in most cases is to leave `Active` as `false`, and to set the [AutoActive](../BoldSystemHandle/TBoldSystemHandle.md#autoactivate) property instead. but if the applicaiton needs any kind of initialization before the system is activated, then it can be activated manually. The system will be deactivated automatically when the form/datamodule it belongs to is closed/destroyed, but can also be manually shut down by setting this property to false.

**Bold Events**  
Assigning to `Active` can result in the the following events:

- **beValueIdentityChanged**: Will be sent whenever `Active` changes value, i.e. the system is created or destroyed.

| **Note** |
|---|

| If the system has any dirty objects when it is shut down it will raise an exception. The correct thing to do is to either save the dirty objects, or discard the system (or ask the user what he prefers). |
|---|

### IsDefault

```delphi
property IsDefault: Boolean;
```

Set this property to `true` to specify that the system is the default system. The default system is used in all cases where no explicit system has been given. Note that the concept of default system can be ill-defined at designtime if several Bold projects are open in Delphi:s IDE.

If a application contains several system handles with `IsDefault` `true`, one of them will be used as the default system.

In general it is recomended that applications with only one system mark it as the default, while applications with several systems always give an explicit system, and never use a default.

Changing the default system at runtime is definitely not recommended.

### System

```delphi
property System: TBoldSystem;
```

It has the same value as [Value](TBoldElementHandle.md#value), but is correctly typed. If [Active](TBoldAbstractSystemHandle.md#active) is `false`, the `System` is `**nil**`.

**Bold events**  
Since `System` is a read only property setting it can't directly give rise to events. The following event will however be sent when the system is created or destroyed:

- **beValueIdentityChanged**: Sent when the system is created or destroyed, i.e. when [Active](TBoldAbstractSystemHandle.md#active) changes its value.

### SystemTypeInfoHandle

```delphi
property SystemTypeInfoHandle: TBoldSystemTypeInfoHandle;
```

This property must refer to a `TBoldSystemTypeInfoHandle` which is in turn connected to the model for the system. Several handles can share the same `TBoldSystemTypeInfoHandle`, and will in that case be type compatible. This property may not be modified when the system is [Active](TBoldAbstractSystemHandle.md#active).

**Bold events**  
Since changing `SystemTypeInfoHandle` will change `StaticSystemTypeInfo` and `StaticBoldType` it gives rise to the following events:

- **beValueIdentityChanged**: Will be sent if `SystemTypeInfoHandle` is assigned a new value.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Create](#create) | Creates an instance of TBoldAbstractSystemHandle | override |
| [DefaultBoldSystemHandle](#defaultboldsystemhandle) | The default TBoldSystemHandle if there is one |  |
| [DefaultBoldSystemTypeInfo](#defaultboldsystemtypeinfo) | Obsolete |  |
| [Destroy](#destroy) | Destroys an instance of TBoldAbstractSystemHandle | override |
| [GetActive](#getactive) | Get-method for the Active property | protected, abstract |
| [GetStaticBoldType](#getstaticboldtype) | Overrides TBoldElementHandle.GetStaticBoldType | protected, override |
| [GetStaticSystemTypeInfo](#getstaticsystemtypeinfo) | Overrides TBoldElementHandle.GetStaticSystemTypeInfo | protected, override |
| [GetSystem](#getsystem) | Get-method for the System property | protected, abstract |
| [ModelChanged](#modelchanged) | Bold-internal | protected, abstract |
| [RefersToComponent](#referstocomponent) | Overrides TBoldElementHandle.RefersToComponent | override |
| [SetActive](#setactive) | Set-method for the Active property | protected, abstract |

### Create

```delphi
constructor Create(owner: TComponent); override;
```

Create creates an instance of `TBoldAbstractSystemHandle`.

There is never any need to create an instance of the abstract class `TBoldAbstractSystemHandle`. [TBoldSystemHandle](../BoldSystemHandle/TBoldSystemHandle.md) is a more usable subclass.

### DefaultBoldSystemHandle

```delphi
class function DefaultBoldSystemHandle: TBoldAbstractSystemHandle;
```

This class method returns the default `TBoldSystemHandle` if there is one. This is defined by the property [IsDefault](TBoldAbstractSystemHandle.md#isdefault).

### DefaultBoldSystemTypeInfo

```delphi
class function DefaultBoldSystemTypeInfo: TBoldSystemTypeInfo;
```

| **Warning** |
|---|

| This property is not used, and will be removed in future releases. |
|---|

### Destroy

```delphi
destructor Destroy; override; See also Ancestor Method
```

`Destroy` destroys an instance of `TBoldAbstractSystemHandle`. Do not call `Destroy` directly, use `Free` instead.

### GetActive

```delphi
function GetActive: Boolean; virtual; abstract;
```

Get-method for the [Active](TBoldAbstractSystemHandle.md#active) property

### GetStaticBoldType

```delphi
function GetStaticBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

Overrides [GetStaticBoldType](TBoldElementHandle.md#getstaticboldtype)

### GetStaticSystemTypeInfo

```delphi
function GetStaticSystemTypeInfo: TBoldSystemTypeInfo; override; See also Ancestor Method
```

Overrides [GetStaticSystemTypeInfo](TBoldElementHandle.md#getstaticsystemtypeinfo)

### GetSystem

```delphi
function GetSystem: TBoldSystem; virtual; abstract;
```

Get-method for the [System](TBoldAbstractSystemHandle.md#system) property

### ModelChanged

```delphi
procedure ModelChanged; virtual; abstract;
```

Bold-internal

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

Overrides [RefersToComponent](TBoldElementHandle.md#referstocomponent)

### SetActive

```delphi
procedure SetActive(Value: Boolean); virtual; abstract;
```

Set-method for the [Active](TBoldAbstractSystemHandle.md#active) property

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
