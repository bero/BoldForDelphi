# TBAInteger

An integer attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAInteger = class(TBANumeric)
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
10. TBAInteger
11. **Direct subclasses**
12. [TBASmallInt](TBASmallInt.md)
13. [TBAWord](TBAWord.md)

## Description

This attribute type is used to store integer values (-2147483648..2147483647) (`Low(integer)..High(integer)`). It is also the base type of all the other bold attribute types that represent values from the integer range.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsInteger](#asinteger) | The actual integer value |  |

### AsInteger

```delphi
property AsInteger: integer;
```

This is the native delphi representation of the integer value.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CheckRange](#checkrange) | Ensure the value is within the legal range | virtual |
| [CheckRangeWithBounds](#checkrangewithbounds) |  | protected |
| [CompareToAs](#comparetoas) |  | override |
| [GetAsFloat](#getasfloat) |  | protected, override |
| [GetAsInteger](#getasinteger) |  | protected, virtual |
| [GetAsVariant](#getasvariant) |  | override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsInteger](#setasinteger) |  | protected, override |
| [SetAsVariant](#setasvariant) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [ValidateCharacter](#validatecharacter) |  | override |
| [ValidateString](#validatestring) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

This will copy the integer value from another integer value

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
function CanSetValue(Value: integer; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CheckRange

```delphi
function CheckRange(Value: integer): Boolean; virtual;
```

| **Note** |
|---|

| CheckRange |
|---|

will return `true` if the value is within (`Low(integer)..High(integer)`).

### CheckRangeWithBounds

```delphi
function CheckRangeWithBounds(Value, Min, Max: integer): boolean;
```

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

This will compare the `TBAInteger` to any numeric value with the following result codes:

- `self.AsInteger < TBAInteger(BoldElement).AsInteger` returns -1
- `self.AsInteger = TBAInteger(BoldElement).AsInteger` returns 0
- `self.AsInteger > TBAInteger(BoldElement).AsInteger` returns 1

### GetAsFloat

```delphi
function GetAsFloat: Double; override; See also Ancestor Method
```

### GetAsInteger

```delphi
function GetAsInteger: integer; virtual;
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
function MaySetValue(NewValue: integer; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBAInteger` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
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

Returns `True` if C is one of '0'..'9', '-' or '+'

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

Returns `true` if the string can be converted to an integer, and if the result is a valid value for the current subclass (subclasses mey restrict the valid range by overriding the `CheckRange` method)

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
