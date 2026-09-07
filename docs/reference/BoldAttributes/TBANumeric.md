# TBANumeric

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBANumeric = class(TBoldAttribute)
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
9. TBANumeric
10. **Direct subclasses**
11. [TBACurrency](TBACurrency.md)
12. [TBAFloat](TBAFloat.md)
13. [TBAInteger](TBAInteger.md)

## Description

This is an abstract superclass of all numeric attribute types in Bold. It can be read as a float value, and written as an integer value (not all numeric types can store float values).

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsFloat](#asfloat) | The value of the attribute, represented as a Double | read-only |
| [AsInteger](#asinteger) |  |  |

### AsFloat

```delphi
property AsFloat: Double;
```

This is the numeric value of the attribute. Note that it can only be read, not written.

### AsInteger

```delphi
property AsInteger: Integer;
```

This property is write only. Since some numeric types contain decimals, you must do the rounding yourself if you need to read an integer value from a `TBANumeric`. Use either `round(asFloat)` or `trunc(asFloat)`.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [GetAsFloat](#getasfloat) |  | protected, abstract |
| [SetAsInteger](#setasinteger) |  | protected, abstract |
| [SetEmptyValue](#setemptyvalue) |  | override |

### GetAsFloat

```delphi
function GetAsFloat: Double; virtual; abstract;
```

### SetAsInteger

```delphi
procedure SetAsInteger(Value: integer); virtual; abstract;
```

### SetEmptyValue

```delphi
procedure SetEmptyValue; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
