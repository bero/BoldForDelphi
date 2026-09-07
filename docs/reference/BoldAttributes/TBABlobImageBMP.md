# TBABlobImageBMP

A .bmp image

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBABlobImageBMP = class(TBABlob)
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
9. [TBABlob](TBABlob.md)
10. TBABlobImageBMP

## Description

The [ContentType](TBABlob.md#contenttype) of this blob is 'image/bitmap'.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
