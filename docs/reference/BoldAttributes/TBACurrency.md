# TBACurrency

A `currency` attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBACurrency = class(TBANumeric)
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
10. TBACurrency

## Description

This attribute uses the delphi Currency type to represent a decimal point value. The advantages of Currency is better handling of rounding-effects in monetary calculations. It is stored as a 64-bit integer with the last 4 decimal digits representing decimal places.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsCurrency](#ascurrency) |  |  |
| [AsFloat](#asfloat) |  |  |
| [AsInteger](#asinteger) |  |  |

### AsCurrency

```delphi
property AsCurrency: Currency;
```

This is the native delphi represenation of the currency value, it is both read- and writeable.

### AsFloat

```delphi
property AsFloat: Double;
```

This property is the currency value converted to a delphi float (`double`). It can be both read and written. Writing this property may introduce unwanted rounding effects.

### AsInteger

```delphi
property AsInteger: Integer;
```

This property is only writeable, since reading this property would loose all the decimals. If you want the integer value of a `TBACurrency`, you should use round(AsCurrency) or trunc(AsCurrency).

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [GetAsCurrency](#getascurrency) |  | protected |
| [GetAsFloat](#getasfloat) |  | protected, override |
| [GetAsVariant](#getasvariant) |  | override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsCurrency](#setascurrency) |  | protected |
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

This will copy the numeric value of any [TBANumeric](TBANumeric.md) subclass (using the `AsCurrency` value if the source is a [TBACurrency](TBACurrency.md)).

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
function CanSetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

This will compare the numeric values of the `TBACurrency` itself and the compare element.

These are the result codes:

- `self.AsCurrency < BoldElement` returns -1
- `self.AsCurrency = BoldElement` returns 0
- `self.AsCurrency > BoldElement` returns 1

If `BoldElement` is a `TBACurrency`, the `AsCurrency` (or subclass) will be used when comparing. If `BoldElement` is a `TBAFloat` (or subclass), the `AsFloat` property will be used.

### GetAsCurrency

```delphi
function GetAsCurrency: Currency;
```

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
function MaySetValue(NewValue: Currency; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBACurrency` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetAsCurrency

```delphi
procedure SetAsCurrency(Value: Currency);
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

Returns `True` if C is one of '0'..'9', '-', '+', 'e', 'E' or the decimal separator as specified by the OS.

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `true` if the string can be converted to a currency. The method uses Delphi's internal routines to determine this.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
