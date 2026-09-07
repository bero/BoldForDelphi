# EBoldFailure

Raised because an operation was not allowed to be performed

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
EBoldFailure = class(EBold)
```

## Hierarchy

1. Exception
2. `EBold`
3. EBoldFailure
4. **Direct subclasses**
5. `EBoldGetLocksFailed`

## Description

This exception is raised because an operation was not allowed to be performed. The ReasonObject property contains a description of the reason for the failure. This type of exception is raised when an operation fails due to a veto of some kind. A vetoing function, such as [MayModify](TBoldMember.md#maymodify), does not itself raise an exception, but instead returns `false` to prohibit the operation. The `ReasonObject` is then used to say why the operation wasn't allowed.

## Properties

| Name | Summary | Notes |
|---|---|---|
| [ReasonObject](#reasonobject) | The reason for the failure |  |

### ReasonObject

```delphi
property ReasonObject: TBoldFailureReason;
```

Object describing the reason for the failure.

## Methods

| Name | Summary | Notes |
|---|---|---|
| [destroy](#destroy) | Destructor | override |

### destroy

```delphi
destructor destroy; override;
```

Destructor

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
