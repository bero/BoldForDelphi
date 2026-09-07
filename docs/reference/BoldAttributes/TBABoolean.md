# TBABoolean

The class of boolean attributes

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBABoolean = class(TBAValueSet)
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
10. TBABoolean
11. **Direct subclasses**
12. [TBAConstraint](TBAConstraint.md)

## Description

This attribute type can store a boolean value. It has three different string representations:

- brDefault (1) - 'N', 'Y'
- 2 - 'F', 'T'
- 3 - 'False', 'True'

If you want other string-representations, the easiest way is to subclass `TBABoolean`, and override the `GetStringRepresentation`-method

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsBoolean](#asboolean) |  |  |

### AsBoolean

```delphi
property AsBoolean: Boolean;
```

This property is the actual delphi-representation of the boolean value stored in the attribute.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [GetAsBoolean](#getasboolean) | Getter for the AsBoolean property | protected |
| [GetAsVariant](#getasvariant) | Returns the data state as a variant | override |
| [GetValues](#getvalues) | Returns the TBAValueSetValueList for the Boolean property | protected, override |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsBoolean](#setasboolean) | Set method for AsBoolean | protected |
| [SetAsVariant](#setasvariant) | Sets the native value from a variant | override |

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); override; See also Ancestor Method
```

### GetAsBoolean

```delphi
function GetAsBoolean: Boolean;
```

`GetAsBoolean` checks if reading is permitted using [CanRead](../BoldSystem/TBoldMember.md#canread) before returning the value held by the attribute.

### GetAsVariant

```delphi
function GetAsVariant: Variant; override; See also Ancestor Method
```

`GetAsVariant` simply returns the [AsBoolean](TBABoolean.md#asboolean) result as a variant.

### GetValues

```delphi
function GetValues: TBAValueSetValueList; override; See also Ancestor Method
```

Since `TBABoolean` is implemented as a subclass of [TBAValueSet](TBAValueSet.md) it is related to a list of valid values. This method returns the list. The list is a singleton, not created until accessed and automatically destroyed on termination.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetAsBoolean

```delphi
procedure SetAsBoolean(Value: Boolean);
```

`SetAsBoolean` is the set method for the [AsBoolean](TBABoolean.md#asboolean) property. It ensures the value can be set prior to altering it.

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); override; See also Ancestor Method
```

`SetAsVariant` sets the [AsBoolean](TBABoolean.md#asboolean) property with the variant passed as parameter.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
