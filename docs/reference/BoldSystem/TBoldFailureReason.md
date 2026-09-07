# TBoldFailureReason

Instances of this class contain information about why a certain operation was not permitted.

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldFailureReason = class(TObject)
```

## Hierarchy

1. TObject
2. TBoldFailureReason
3. **Direct subclasses**
4. `TBoldFailureGetLocksFailed`

## Description

Instances of this class contain information about why a certain operation was not permitted.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [MessageFormatStr](#messageformatstr) | BoldRaiseLastFailure uses this message for building the exception message. |  |
| [Originator](#originator) | Domain element that caused the failure. | read-only |
| [Reason](#reason) | String describing the failure | read-only |

### MessageFormatStr

```delphi
property MessageFormatStr: String;
```

BoldRaiseLastFailure uses this message for building the exception message.

The actual message of the exception that will be raised will be created by sending property to `format` with the following arguments:

| format(MessageFormatString, [OriginatorName, MethodName, **Message**]); |
|---|

### Originator

```delphi
property Originator: TBoldDomainElement;
```

Domain element that caused the failure.

### Reason

```delphi
property Reason: string;
```

String describing the failure

## Methods

| Name | Summary | Notes |
|---|---|---|
| [create](#create) | Constructor with reason and originator |  |
| [CreateFmt](#createfmt) | Constructor with Format arguments |  |
| [destroy](#destroy) | Destructor | override |
| [GetException](#getexception) | The class of the exception to raise in BoldRaiseLastFailure. | protected, virtual |

### create

```delphi
constructor create(reason: String; Originator: TBoldDomainElement);
```

Constructor with reason and originator. `Reason` is a string describing the failure. `Originator` is the domain element that caused the failure, and may be `**nil**`.

### CreateFmt

```delphi
constructor CreateFmt(Reason: string; const args: array of const; Originator: TBoldDomainElement);
```

The `Reason` and `args` arguments work the same way as for the `Format` function. `Originator` is domain element that caused the failure, and may be `**nil**`.

### destroy

```delphi
destructor destroy; override;
```

Destructor

### GetException

```delphi
function GetException(const Msg: String): EBoldFailure; virtual;
```

Subclasses may override this method to provide the class of the exception to raise when `BoldRaiseLastFailure` is called.

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
