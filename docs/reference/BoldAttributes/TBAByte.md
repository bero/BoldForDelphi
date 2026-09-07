# TBAByte

This attribute can store integer values in the range 0..255.

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAByte = class(TBAWord)
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
11. [TBAWord](TBAWord.md)
12. TBAByte

## Description

This attribute can store integer values in the range 0..255.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsByte](#asbyte) |  |  |

### AsByte

```delphi
property AsByte: Byte;
```

The native delphi representation of the byte value.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CheckRange](#checkrange) |  | override |

### CheckRange

```delphi
function CheckRange(Value: integer): Boolean; override; See also Ancestor Method
```

Raises an exception if the parameter is not a valid byte (0..255)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
