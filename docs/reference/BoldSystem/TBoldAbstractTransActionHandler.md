# TBoldAbstractTransActionHandler

Bold-internal

!!! warning "Not in the current source"
    This class is documented in the Bold 4.0 help, but no class or interface with this name
    is declared in `Source/` today. It was removed or reshaped (for example into a record).

**Unit**: [BoldSystem](index.md)

## Declaration

```delphi
TBoldAbstractTransActionHandler = class(TBoldAbstractOldValueHandler)
```

## Hierarchy

1. TObject
2. `TBoldMemoryManagedObject`
3. `TBoldInterfacedObject`
4. `TBoldNonRefCountedObject`
5. [TBoldSystemExtension](TBoldSystemExtension.md)
6. [TBoldAbstractOldValueHandler](TBoldAbstractOldValueHandler.md)
7. TBoldAbstractTransActionHandler

## Description

Bold-internal

## Properties

| Name | Summary | Notes |
|---|---|---|
| [TransactionMode](#transactionmode) |  | read-only |
| [TransactionState](#transactionstate) |  | read-only |

### TransactionMode

```delphi
property TransactionMode: TBoldSystemTransactionMode;
```

### TransactionState

```delphi
property TransactionState: TBoldTransactionState;
```

## Methods

| Name | Summary | Notes |
|---|---|---|
| [CommitTransaction](#committransaction) |  | abstract |
| [GetTransactionMode](#gettransactionmode) |  | protected, abstract |
| [GetTransactionState](#gettransactionstate) |  | protected, abstract |
| [RollbackTransaction](#rollbacktransaction) |  | abstract |
| [StartTransaction](#starttransaction) |  | abstract |

### CommitTransaction

```delphi
procedure CommitTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
```

### GetTransactionMode

```delphi
function GetTransactionMode: TBoldSystemTransactionMode; virtual; abstract;
```

### GetTransactionState

```delphi
function GetTransactionState: TBoldTransactionState; virtual; abstract;
```

### RollbackTransaction

```delphi
procedure RollbackTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
```

### StartTransaction

```delphi
procedure StartTransaction(MinimalMode: TBoldSystemTransactionMode = stmNormal); virtual; abstract;
```

---

*Generated from the Bold 4.0 help (`Help/BfD.chm`) by `Tools/chm2mkdocs.py`; member lists reflect Bold 4.0, see the source for members added since.*
