# TBAValueSet

Subclass `TBAValueSet` to create enumeration type attributes.

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAValueSet = class(TBoldAttribute)
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
9. TBAValueSet
10. **Direct subclasses**
11. [TBABoolean](TBABoolean.md)
12. `TBALanguage`
13. `TBAMLValueSet`

## Description

Subclass `TBAValueSet` to create enumeration type attributes. The attribute wizard (accessible through the "Bold | Bold Attribute Wizard" menu) is the easiest way to create custom attributes.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [AsInteger](#asinteger) |  |  |
| [ContentAsInteger](#contentasinteger) |  | protected |
| [Values](#values) |  | read-only |

### AsInteger

```delphi
property AsInteger: Integer;
```

### ContentAsInteger

```delphi
property ContentAsInteger: Integer;
```

### Values

```delphi
property Values: TBAValueSetValueList;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [CompareToEnumLiteral](#comparetoenumliteral) | Compares the valueset to an enum literal string | virtual |
| [GetAsVariant](#getasvariant) |  | override |
| [GetContentAsInteger](#getcontentasinteger) |  | protected, virtual |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [GetValues](#getvalues) |  | protected, abstract |
| [InitializeMember](#initializemember) |  | protected, override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetAsVariant](#setasvariant) |  | override |
| [SetEmptyValue](#setemptyvalue) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [ValidateCharacter](#validatecharacter) |  | override |
| [ValidateString](#validatestring) |  | override |

### Assign

```delphi
procedure Assign(Source: TBoldElement); override; See also Ancestor Method
```

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
function CanSetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

### CompareToEnumLiteral

```delphi
function CompareToEnumLiteral(const str: String): Boolean; virtual;
```

This function is used internally by the OCL-evaluator

### GetAsVariant

```delphi
function GetAsVariant: Variant; override; See also Ancestor Method
```

### GetContentAsInteger

```delphi
function GetContentAsInteger: Integer; virtual;
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### GetValues

```delphi
function GetValues: TBAValueSetValueList; virtual; abstract;
```

### InitializeMember

```delphi
procedure InitializeMember(OwningElement: TBoldDomainElement; ElementTypeInfo: TBoldElementTypeInfo); override; See also Ancestor Method
```

### MaySetValue

```delphi
function MaySetValue(NewValue: TBAValueSetValue; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBAValueSet` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetAsVariant

```delphi
procedure SetAsVariant(const Value: Variant); override; See also Ancestor Method
```

### SetEmptyValue

```delphi
procedure SetEmptyValue; override; See also Ancestor Method
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

### ValidateCharacter

```delphi
function ValidateCharacter(C: AnsiChar; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
