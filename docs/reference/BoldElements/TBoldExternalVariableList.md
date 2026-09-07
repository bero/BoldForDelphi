# TBoldExternalVariableList

This class is used to send external variables to OCL expressions

**Unit**: [BoldElements](index.md)

## Declaration

```delphi
TBoldExternalVariableList = class(TBoldObjectArray)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. `TBoldContainer`
6. `TBoldArray`
7. `TBoldObjectArray`
8. TBoldExternalVariableList

## Description

The methods of [TBoldElement](TBoldElement.md) that evaluate OCL expressions all take a parameter called `Variables`. That parameter should be an instance of this class filled with [external variables](TBoldExternalVariable.md) (or nil)

## Properties

| Name | Summary | Notes |
|---|---|---|
| [Variables](#variables) |  | read-only |

### Variables

```delphi
property Variables[index:integer]: TBoldExternalVariable;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [Add](#add) | Adds an external variable to the list |  |
| [create](#create) |  |  |

### Add

```delphi
procedure Add(Variable: TBoldExternalVariable);
```

The variable list will assume ownership of the external variable and take care of its destruction

### create

```delphi
constructor create;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
