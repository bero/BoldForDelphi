# TBAShortInt

A shortint attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAShortInt = class(TBASmallInt)
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
9. [TBANumeric](TBANumeric.md)
10. [TBAInteger](TBAInteger.md)
11. [TBASmallInt](TBASmallInt.md)
12. TBAShortInt

## Description

A `shortint` stores values between -128 and 127 (`Low(ShortInt)..High(ShortInt)`).

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsShortInt](#asshortint) | The actual shortint value |  |

### AsShortInt

```delphi
property AsShortInt: ShortInt;
```

The native delphi representation of a `shortint`

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CheckRange](#checkrange) |  | override |

### CheckRange

```delphi
function CheckRange(Value: integer): Boolean; override; See also Ancestor Method
```

Returns true for values between (-128..127) (`Low(ShortInt)..High(ShortInt)`)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
