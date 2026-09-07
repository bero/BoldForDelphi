# TBAString

A string attribute

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBAString = class(TBoldAttribute)
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
9. TBAString
10. **Direct subclasses**
11. `TBAMLString`
12. `TBAMLSubString`
13. [TBATrimmedString](TBATrimmedString.md)

## Description

This attribute type is used to store strings.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Assign](#assign) |  | override |
| [AssignContentValue](#assigncontentvalue) |  | protected, override |
| [AssignValue](#assignvalue) |  | override |
| [CanSetValue](#cansetvalue) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [FreeContent](#freecontent) |  | protected, override |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [MaySetValue](#maysetvalue) | Override to affect the result of CanSetValue . | protected, virtual |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetEmptyValue](#setemptyvalue) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
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
function CanSetValue(NewValue: string; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

This will compare the `TBAString` to any string value with the following result codes:

- `self.AsString < BoldElement.AsString` returns -1
- `self.AsString = BoldElement.AsString` returns 0
- `self.AsString > BoldElement.AsString` returns 1

The `CompType` will decide how the strings will be compared:

- `ctDefault`, `ctAsAnsiText`: AnsiCompareText
- `ctAsAnsiString`: AnsiCompareStr
- `ctAsString`: CompareStr
- `ctAsText`: CompareText

### FreeContent

```delphi
procedure FreeContent; override; See also Ancestor Method
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### MaySetValue

```delphi
function MaySetValue(NewValue: String; Subscriber: TBoldSubscriber): Boolean; virtual;
```

A subclass of `TBAString` may override this method to prohibit setting the attribute to a specific value. If the result value is `false`, the setting will be aborted. If it is `true`, there may still be other reasons why the changing of the value is not allowed.

### ProxyClass

```delphi
function ProxyClass: TBoldMember_ProxyClass; override; See also Ancestor Method
```

### ProxyInterface

```delphi
function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override; See also Ancestor Method
```

### SetEmptyValue

```delphi
procedure SetEmptyValue; override; See also Ancestor Method
```

### SetStringRepresentation

```delphi
procedure SetStringRepresentation(Representation: TBoldRepresentation; Value: string); override; See also Ancestor Method
```

### ValidateString

```delphi
function ValidateString(Value: string; Representation: TBoldRepresentation): Boolean; override; See also Ancestor Method
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
