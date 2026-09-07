# TBATime

A time attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBATime = class(TBAMoment)
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
10. TBATime

## Description

This attribute can be used to store a time. Note that even though the delphi type `TTime` is able to store dates, the bold attribute `TBATime` will remove this information before storing times.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsSeconds](#asseconds) | The time expressed as seconds | read-only |
| [AsTime](#astime) | The actual TTime value |  |
| [Hours](#hours) |  | read-only |
| [Minutes](#minutes) |  | read-only |
| [Seconds](#seconds) |  | read-only |

### AsSeconds

```delphi
property AsSeconds: cardinal;
```

`AsSeconds` returns the time value expressed as seconds, using the formula

| AsSeconds := Seconds + (Minutes * 60) + (Hours * 3600) |
|---|

**See Also**

- [Hours](TBATime.md#hours)
- [Minutes](TBATime.md#minutes)
- [Seconds](TBATime.md#seconds)

### AsTime

```delphi
property AsTime;
```

This property holds the native Delphi representation of the attribute.

### Hours

```delphi
property Hours;
```

### Minutes

```delphi
property Minutes;
```

### Seconds

```delphi
property Seconds;
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

Returns `True` if C is one of '0'..'9' or time separator.

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

`True` if the string can be converted to a time.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
