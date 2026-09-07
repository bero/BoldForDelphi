# TBADateTime

A datetime attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBADateTime = class(TBAMoment)
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
10. TBADateTime

## Description

This attribute can be used to store a date and a time. The date and time can be read and set speparately by using `AsDate` and `AsTime`, or together with the `AsDateTime`. Note that this differ somewhat from how delphi regards `TDate` and `TTime` (to Delphi they are just new names for `TDateTime`).

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsDate](#asdate) | The date portion of the attribute |  |
| [AsDateTime](#asdatetime) | The actual datetime value of the attribute |  |
| [AsTime](#astime) | The time portion of the attribute |  |
| [Days](#days) |  | read-only |
| [Hours](#hours) |  | read-only |
| [Minutes](#minutes) |  | read-only |
| [Months](#months) |  | read-only |
| [Seconds](#seconds) |  | read-only |
| [Years](#years) |  | read-only |

### AsDate

```delphi
property AsDate;
```

This property holds the date portion of the attribute, in native delphi format. It can be both read and written.

### AsDateTime

```delphi
property AsDateTime;
```

This property holds the actual datetime value of the attribute. It can be both read and written.

### AsTime

```delphi
property AsTime;
```

This property holds the time portion of the attribute

### Days

```delphi
property Days;
```

### Hours

```delphi
property Hours;
```

### Minutes

```delphi
property Minutes;
```

### Months

```delphi
property Months;
```

### Seconds

```delphi
property Seconds;
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

Returns `True` if C is one of '0'..'9', space, time separator or date separator

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `true` if the string can be converted to a date, a time or both.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
