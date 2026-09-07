# TBATypedBlob

**Unit**: [BoldAttributes](index.md)

## Declaration

```delphi
TBATypedBlob = class(TBABlob)
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
10. TBATypedBlob

## Description

This subclass of `TBABlob` adds the possibility to set the content type of the data. The content type is any string that identifies the format of the data, such as the Mime-type identifiers:

- image/bmp
- image/jpeg
- image/x-wmf
- audio/wav
- text/html
- text/xml

The attribute itself knows nothing about mimetypes, and will enforce no control of the correctness of data, this must be added by new subclasses.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [AssignValue](#assignvalue) |  | override |
| [CanSetContentType](#cansetcontenttype) |  |  |
| [CompareToAs](#comparetoas) |  | override |
| [GetContentTypeContent](#getcontenttypecontent) |  | protected |
| [GetStringRepresentation](#getstringrepresentation) |  | protected, override |
| [IsEqualAs](#isequalas) |  | override |
| [ProxyClass](#proxyclass) |  | protected, override |
| [ProxyInterface](#proxyinterface) |  | override |
| [SetStringRepresentation](#setstringrepresentation) |  | protected, override |
| [SetToNull](#settonull) |  | override |

### AssignValue

```delphi
procedure AssignValue(Source: IBoldValue); override; See also Ancestor Method
```

### CanSetContentType

```delphi
function CanSetContentType(Value: string; Subscriber: TBoldSubscriber): Boolean;
```

Use this function to test if it is allowed to set the attribute to a specific value.

If a subscriber is sent to the function, this subscriber will be notified if the conditions have changed (so that an illegal value is now legal). It is OK to send a nilpointer instead.

### CompareToAs

```delphi
function CompareToAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Integer; override; See also Ancestor Method
```

### GetContentTypeContent

```delphi
function GetContentTypeContent: String;
```

### GetStringRepresentation

```delphi
function GetStringRepresentation(Representation: TBoldRepresentation): string; override; See also Ancestor Method
```

### IsEqualAs

```delphi
function IsEqualAs(CompareType: TBoldCompareType; BoldElement: TBoldElement): Boolean; override; See also Ancestor Method
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

### SetToNull

```delphi
procedure SetToNull; override; See also Ancestor Method
```

In addition to what is done by the inherited method (setting the attribute to null), it will also free the memory used by the content type string.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
