# TBoldCheckBox

`TBoldCheckBox` is a data-aware checkbox that displays and edits a Boolean Bold attribute. It supports three states (checked, unchecked, grayed) for nullable boolean attributes.

## Class Hierarchy

```mermaid
classDiagram
    TCustomCheckBox <|-- TBoldCustomCheckBox
    TBoldCustomCheckBox <|-- TBoldCheckBox

    TBoldCheckBox : +BoldHandle
    TBoldCheckBox : +BoldProperties
    TBoldCheckBox : +Checked
    TBoldCheckBox : +State
    TBoldCheckBox : +ReadOnly

    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Handle providing the boolean value |
| `BoldProperties` | `TBoldCheckBoxStateFollowerController` | Controls state mapping |
| `ReadOnly` | `Boolean` | Prevent toggling |
| `AllowGrayed` | `Boolean` | Allow three-state for nullable booleans |
| `Expression` | `string` | OCL expression |

| Public Property | Type | Description |
|-----------------|------|-------------|
| `Checked` | `Boolean` | Current checked state (read-only from Bold) |
| `State` | `TCheckBoxState` | `cbChecked`, `cbUnchecked`, `cbGrayed` |
| `EffectiveReadOnly` | `Boolean` | Combines ReadOnly + model constraints |

!!! note "Uses TBoldCheckBoxStateFollowerController"
    Unlike most Bold controls that use `TBoldStringFollowerController`, the checkbox uses `TBoldCheckBoxStateFollowerController` which maps directly to checkbox states.

## Working with TBoldCheckBox

### Form Designer Setup

1. Drop `TBoldCheckBox` on a form
2. Set `BoldHandle` to an expression handle pointing to a boolean attribute
3. Optionally enable `AllowGrayed` for nullable booleans

### Nullable Booleans (Three-State)

```
AllowGrayed = True

cbChecked   → True
cbUnchecked → False
cbGrayed    → Null (unknown)
```

This is useful when a boolean attribute hasn't been set yet and you want to distinguish "not answered" from "no".

### Example Setup

```pascal
// Expression handle points to the boolean attribute
ExprHandle.RootHandle := CustomerListHandle;
ExprHandle.Expression := 'active';

// Connect checkbox
BoldCheckBox1.BoldHandle := ExprHandle;
BoldCheckBox1.Caption := 'Active Customer';
BoldCheckBox1.AllowGrayed := True;  // for nullable
```

## See Also

- [TBABoolean](TBABoolean.md) - The boolean attribute class
- [TBoldEdit](TBoldEdit.md) - Text editing
- [TBoldGrid](TBoldGrid.md) - Grids with `AllowCheckBox` columns
