# TBASmallInt

Attribute type corresponding to SmallInt

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBASmallInt = class(TBAInteger)
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
11. TBASmallInt
12. **Direct subclasses**
13. [TBAShortInt](TBAShortInt.md)

## Description

Stores values between -32768..32767 (`Low(SmallInt)..High(SmallInt)`)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsSmallInt](#assmallint) | The actual smallint value |  |

### AsSmallInt

```delphi
property AsSmallInt: SmallInt;
```

The native Delphi representation of a `smallint`

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CheckRange](#checkrange) |  | override |

### CheckRange

```delphi
function CheckRange(Value: integer): Boolean; override; See also Ancestor Method
```

Returns `true` for values between -32768..32767 (`Low(SmallInt)..High(SmallInt)`)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
