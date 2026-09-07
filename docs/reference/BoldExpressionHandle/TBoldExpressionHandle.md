# TBoldExpressionHandle

A handle that uses an OCL expression to evaluate a value from its root handle.

**Unit**: [BoldExpressionHandle](index.md)

## Declaration

```delphi
TBoldExpressionHandle = class(TBoldRootedHandle, IBoldOCLComponent)
```

## Hierarchy

1. TComponent
2. [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md)
3. [TBoldElementHandle](../BoldHandles/TBoldElementHandle.md)
4. [TBoldNonSystemHandle](../BoldHandles/TBoldNonSystemHandle.md)
5. [TBoldRootedHandle](../BoldRootedHandles/TBoldRootedHandle.md)
6. TBoldExpressionHandle

## Description

`TBoldExpressionHandle` is a subclass of [TBoldRootedHandle](../BoldRootedHandles/TBoldRootedHandle.md) and everything in its description applies and is for the most part not repeated here.

**Value**  
The value of the handle is defined by evaluating [Expression](TBoldExpressionHandle.md#expression) with `RootHandle.Value` as the context.

**Typing**  
The `StaticBoldType` of the handle is defined as the type of `Expression` in the context of `StaticRootType`. If the handle is connected to another handle `StaticRootType` will the the `StaticBoldType` of that handle, otherwise it will be determined through `RootTypeName`.

**Bold Events**  
`TBoldExpressionHandle` is a subclass of [TBoldSubscribableComponent](../BoldSubscription/TBoldSubscribableComponent.md), and can therefor by subscribed to using AddSmallSubscription. A `TBoldExpressionHandle` can send the following events:

- beDestroying: Sent when the handle is about to be destroyed.
- beValueIdentityChanged: Sent when `Value` has changed, i.e. when `Value` points to a new `TBoldElement`. Also sent if anything influencing `StaticBoldType` has changed.

Due to the lazy evaluation, "has changed" has a very specific meaning. It means that the next time the `Value` property is accessed it may return a different value from the previous time. It does not imply that this value has actually been calculated yet.

If several things occur that would change `Value`, but the `Value` property is not accessed in between, only the first will give rise to a `beValueIdentityChanged` event.

| **Note** |
|---|

| The ValueIdentityChanged event is not sent when the contents of Value is changed. This is found out by subscribing to Value itself. |
|---|

## Properties

| Name | Summary | Notes |
|---|---|---|
| [EvaluateInPS](#evaluateinps) | If the expression should be evaluated in the database, instead of in memory |  |
| [Expression](#expression) | Expression is a string containing an OCL expression. |  |
| [Variables](#variables) | Allows the expression to have more than one context |  |

### EvaluateInPS

```delphi
property EvaluateInPS: Boolean;
```

Normally OCL expressions are evaluated in memory, but this can lead to great inefficiencies when numerous objects have to be loaded. If this property is set to true an attempt to execute the expression directly in the database will be made. If this fails, there will be an exception.

Note that if the expression is executed directly against the database, the result will be determined by the contents of the database, and any uncommited data in the object space will be disregarded.

| **Note** |
|---|

| If this property is true, the component will ignore the setting of [Subscribe](../BoldRootedHandles/TBoldRootedHandle.md#subscribe). |
|---|

For further information, see Ocl2Sql

### Expression

```delphi
property Expression: TBoldExpression;
```

The `Value` of the Handle is defined by evaluating `Expression` with `RootHandle.Value` as the context.

At design-time the property editor provides support in writing the expression, provided that either the handle is connected to another handle, or that `RootTypeName` (and optionally `StaticSystemHandle`) is set.

**Bold Events**  
Since changing `Expression` indirectly changes `Value` the following events can be sent when setting `Expression`

- beValueIndentityChanged: Sent if Expression is assigned a new value.

If the property is assigned with the same value as it already has the event will not be sent.

Also, the event will not be sent if it has previously been sent, and the `Value` property has not been accessed since then.

### Variables

```delphi
property Variables: TBoldOclVariables;
```

If the OCL expression requires more than one context, additional context must be imported using a `TBoldOclVariables` component

## Methods

| Name | Summary | Notes |
|---|---|---|
| [create](#create) | Creates an instanc of TBoldExpressionHandle | override |
| [DeriveAndSubscribe](#deriveandsubscribe) |  | protected, override |
| [destroy](#destroy) | Destroys an instance of a TBoldExpressionHandle. | override |
| [GetStaticBoldType](#getstaticboldtype) |  | protected, override |
| [RefersToComponent](#referstocomponent) | Determine if there is a relationship to the component passed as parameter. | override |

### create

```delphi
constructor create(owner: TComponent); override; See also Ancestor Method
```

Use `Create` to create an instance of `TBoldExpressionHandle`.

### DeriveAndSubscribe

```delphi
procedure DeriveAndSubscribe(DerivedObject: TObject; Subscriber: TBoldSubscriber); override; See also Ancestor Method
```

### destroy

```delphi
destructor destroy; override; See also Ancestor Method
```

Do not call `Destroy` directly, use `Free`.

### GetStaticBoldType

```delphi
function GetStaticBoldType: TBoldElementTypeInfo; override; See also Ancestor Method
```

### RefersToComponent

```delphi
function RefersToComponent(Component: TBoldSubscribableComponent): Boolean; override; See also Ancestor Method
```

`RefersToComponent` determins if there is a relationship between the `TBoldExpressionHandle` and the component passed as parameter. It is used internally to avoid circular dependencies between components.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
