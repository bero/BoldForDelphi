# TBoldIndirectElement

Reference to a TBoldElement

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldIndirectElement = class(TBoldFlaggedObject)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldFlaggedObject`
4. TBoldIndirectElement

## Description

A `TBoldIndirectElement` may either own its `Value`, or refer to an existing [TBoldElement](TBoldElement.md) that is owned by someone else. A common use of a `TBoldIndirectElement` is to use it for returning the result of evaluating an expression [Evaluate](TBoldEvaluator.md#evaluate) A TBoldIndirectElement can be created by anyone. When destroyed, it might or might not cascade the destroy to its [value](TBoldIndirectElement.md#value) depending on whether the value is [owned](TBoldIndirectElement.md#ownsvalue) or not (but this is somehting determined by the Evaluate-method, and not the creator of the indirect element.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [OwnsValue](#ownsvalue) | True if the indirect element owns the value it refers to. | read-only |
| [Value](#value) | Referred element of the indirect element | read-only |

### OwnsValue

```delphi
property OwnsValue: Boolean;
```

The [Value](TBoldIndirectElement.md#value) of a `TBoldIndirectElement` can either be an existing [TBoldElement](TBoldElement.md), or one that has been created solely for the purpose of some derivation. In the latter case, the indirect value owns the value, and will be responsible for freeing it.

### Value

```delphi
property Value: TBoldElement;
```

The purpose of an indirect element is to refer to a [TBoldElement](TBoldElement.md), that it either owns or not. The `Value` property holds that element.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Destroy](#destroy) | Destructor | override |
| [RelinquishValue](#relinquishvalue) | Makes the indirect element give up ownership of its value. |  |
| [SetOwnedValue](#setownedvalue) | Sets the value, and passes ownership. |  |
| [SetReferenceValue](#setreferencevalue) | Sets the value, without passing ownership. |  |
| [TransferValue](#transfervalue) | Transfers the value to another indirect element. |  |

### Destroy

```delphi
destructor Destroy; override;
```

Destructor

### RelinquishValue

```delphi
function RelinquishValue: TBoldElement;
```

This function returns the value of the indirect element. It also passes the ownership of the element to the caller. After `RelinquishValue` the indirect element's [Value](TBoldIndirectElement.md#value) property will be `**nil**`

### SetOwnedValue

```delphi
procedure SetOwnedValue(NewValue: TBoldElement);
```

This method will set the indirect element's [Value](TBoldIndirectElement.md#value) property to `NewValue`, and [OwnsValue](TBoldIndirectElement.md#ownsvalue) to true. If the indirect element was already owning another element, that element will be freed.

### SetReferenceValue

```delphi
procedure SetReferenceValue(NewValue: TBoldElement);
```

This method will set the indirect element's [Value](TBoldIndirectElement.md#value) property to `NewValue`, and [OwnsValue](TBoldIndirectElement.md#ownsvalue) to false. If the indirect element was owning another element, that element will be freed.

### TransferValue

```delphi
procedure TransferValue(Target: TBoldIndirectElement);
```

The [Value](TBoldIndirectElement.md#value) element will be transferred to the `Target`. If the indirect element has ownership of the value, ownership will also be passed. After `TransferValue`, the `Value` property will be `**nil**`.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
