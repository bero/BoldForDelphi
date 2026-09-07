# TBADate

A date attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBADate = class(TBAMoment)
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
9. [TBAMoment](TBAMoment.md)
10. TBADate

## Description

This attribute can be used to store a date. Note that even though the delphi type `TDate` is able to store hours and minutes, the bold attribute `TBADate` will remove this information before storing dates.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsDate](#asdate) | Actual date value |  |
| [Days](#days) |  | read-only |
| [Months](#months) |  | read-only |
| [Years](#years) |  | read-only |

### AsDate

```delphi
property AsDate;
```

This is the delphi native representation of the date value. It can be both read and written.

### Days

```delphi
property Days;
```

### Months

```delphi
property Months;
```

### Years

```delphi
property Years;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [ValidateCharacter](#validatecharacter) |  | override |
| [ValidateString](#validatestring) |  | override |

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); override; See also Ancestor Method
```

### AssignValue

```delphi
procedure AssignValue(Source: IBoldValue); override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

### ValidateCharacter

```delphi
function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `True` if C is one of '0'..'9' or the date separator

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `true` if the string can be converted to a date

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
