# TBAFloat

A floating point attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAFloat = class(TBANumeric)
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
10. TBAFloat

## Description

This attribute type can be used to store floating point values.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsFloat](#asfloat) | The actual floating point value |  |
| [AsInteger](#asinteger) |  |  |

### AsFloat

```delphi
property AsFloat: Double;
```

This is the delphi native representation of the floating point value. It can be both read and written.

### AsInteger

```delphi
property AsInteger: Integer;
```

The integer value of a floating point can only be written. If you want to read the integer value you must use either `round(AsFloat)` or `trunc(AsFloat)`.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [GetAsFloat](#getasfloat) |  | protected, override |
| [GetAsVariant](#getasvariant) |  | override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsFloat](#setasfloat) |  | protected, virtual |
| [SetAsInteger](#setasinteger) |  | protected, override |
| [SetAsVariant](#setasvariant) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [ValidateCharacter](#validatecharacter) |  | override |
| [ValidateString](#validatestring) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

This will copy the numeric value from any subclass of `TBANumeric`.

### AssignContentValue

```delphi
procedure AssignContentValue(Source: IBoldValue); override; See also Ancestor Method
```

### AssignValue

```delphi
procedure AssignValue(Source: IBoldValue); override; See also Ancestor Method
```

### CanSetValue

```delphi
function CanSetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

This will compare the `TBAFloat` to any numeric value with the following result codes:

- `self.AsFloat < TBAFloat(BoldElement).AsFloat` returns -1
- `self.AsFloat = TBAFloat(BoldElement).AsFloat` returns 0
- `self.AsFloat > TBAFloat(BoldElement).AsFloat` returns 1

### GetAsFloat

```delphi
function GetAsFloat: Double; override; See also Ancestor Method
```

### GetAsVariant

```delphi
function GetAsVariant: Variant; override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### MaySetValue

```delphi
function MaySetValue(NewValue: Double; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBAFloat` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetAsFloat

```delphi
procedure SetAsFloat(Value: Double); virtual;
```

### SetAsInteger

```delphi
procedure SetAsInteger(Value: integer); override; See also Ancestor Method
```

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); override; See also Ancestor Method
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

### ValidateCharacter

```delphi
function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `True` if C is one of '0'..'9', '-', '+', 'e', 'E' or the decimal separator.

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `true` if the string can be converted to a floating point value.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
