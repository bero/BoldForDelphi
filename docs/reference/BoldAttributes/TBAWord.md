# TBAWord

A word attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAWord = class(TBAInteger)
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
11. TBAWord
12. **Direct subclasses**
13. [TBAByte](TBAByte.md)

## Description

Stores values in the range 0..65535 (Low(word)..High(Word))

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsWord](#asword) | The native delphi representation of a word |  |

### AsWord

```delphi
property AsWord: Word;
```

This property is the native delphi representation of a word.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CheckRange](#checkrange) |  | override |

### CheckRange

```delphi
function CheckRange(Value: integer): Boolean; override; See also Ancestor Method
```

returns `true` for values in the range 0..65535 (`Low(word)..High(Word)`)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
