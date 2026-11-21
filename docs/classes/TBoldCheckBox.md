# TBoldCheckBox

`TBoldCheckBox` is a check box that shows and edits a Boolean Bold attribute (`TBABoolean`). A null value shows as
grayed, so a nullable Boolean has three states.

!!! note "VCL only"
    `TBoldCheckBox` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show Bold
    data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

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
| `BoldHandle` | `TBoldElementHandle` | Handle providing the Boolean value |
| `BoldProperties` | `TBoldCheckBoxStateFollowerController` | Maps the value to a check box state; `BoldProperties.Expression` navigates from the handle's value |
| `ReadOnly` | `Boolean` | Prevent toggling |
| `AllowGrayed` | `Boolean` | Let the user click the box to grayed, which sets the value to null |

| Public Property | Type | Description |
|-----------------|------|-------------|
| `Checked` | `Boolean` | Read-only; `True` when `State` is `cbChecked` |
| `State` | `TCheckBoxState` | `cbChecked`, `cbUnchecked`, `cbGrayed` |
| `EffectiveReadOnly` | `Boolean` | `True` when `ReadOnly` is set or the value cannot be modified |

!!! note "Uses TBoldCheckBoxStateFollowerController"
    Unlike most Bold controls, which use `TBoldStringFollowerController`, the check box uses
    `TBoldCheckBoxStateFollowerController`, which maps the value directly to check box states.

## Working with TBoldCheckBox

### Form Designer Setup

1. Drop a `TBoldCheckBox` on a form.
2. Set `BoldHandle` to an expression handle pointing to a Boolean attribute.
3. Optionally set `AllowGrayed` for a nullable Boolean.

### Nullable Booleans (Three-State)

```text
cbChecked   -> True
cbUnchecked -> False
cbGrayed    -> Null (unknown)
```

A null value always shows as grayed. With `AllowGrayed = True` the user can also set it back to null. This is useful
when you want to tell "not answered" from "no".

### Example Setup

```pascal
// The expression handle points to the Boolean attribute
ExprHandle.RootHandle := CustomerListHandle;
ExprHandle.Expression := 'active';

// Connect the check box
BoldCheckBox1.BoldHandle := ExprHandle;
BoldCheckBox1.Caption := 'Active Customer';
BoldCheckBox1.AllowGrayed := True;  // for a nullable attribute
```

## See Also

- [TBABoolean](TBABoolean.md) - The Boolean attribute class
- [TBoldEdit](TBoldEdit.md) - Text editing
- [TBoldGrid](TBoldGrid.md) - Grids with `AllowCheckBox` columns
