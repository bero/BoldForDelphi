# TBATrimmedString

String attribute that trims leading and trailing spaces.

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBATrimmedString = class(TBAString)
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
9. [TBAString](TBAString.md)
10. TBATrimmedString

## Description

`TBATrimmedString` ensures leading and trailing spaces are removed prior to setting the value.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [SetStringRepresentation](#setstringrepresentation) | Sets the string representation | protected, override |

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

Before setting the string representation, the method trims the value, removing leading and trailing spaces.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
