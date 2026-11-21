# TBoldLabel

`TBoldLabel` is a label that shows a Bold value or the result of an OCL expression as read-only text. It updates
itself when the value changes.

!!! note "VCL only"
    `TBoldLabel` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show Bold
    data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

## Class Hierarchy

```mermaid
classDiagram
    TCustomLabel <|-- TBoldCustomLabel
    TBoldCustomLabel <|-- TBoldLabel

    TBoldLabel : +BoldHandle
    TBoldLabel : +BoldProperties
    TBoldLabel : +Caption (read-only)

    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Handle providing the value |
| `BoldProperties` | `TBoldStringFollowerController` | Rendering; `BoldProperties.Expression` is the OCL expression for the displayed value |
| `Color` | `TColor` | Background color |
| `Font` | `TFont` | Text font |

| Public Property | Description |
|-----------------|-------------|
| `Caption` | Read-only; set from the Bold value |

## Working with TBoldLabel

### Form Designer Setup

1. Drop a `TBoldLabel` on a form.
2. Set `BoldHandle` to an expression handle.
3. Optionally set `BoldProperties.Expression` to navigate from the handle's value.

### Display Patterns

```pascal
// Show a simple attribute
BoldLabel1.BoldHandle := CustomerHandle;
BoldLabel1.BoldProperties.Expression := 'name';

// Show a navigated value
BoldLabel2.BoldProperties.Expression := 'address.city';

// Show a calculated value
BoldLabel3.BoldProperties.Expression := 'orders->size.asString';

// Show a formatted value
BoldLabel4.BoldProperties.Expression := 'salary.formatFloat(''#,##0.00'')';
```

### Conditional Formatting

Put a `TBoldAsStringRenderer` on the form, assign it to `BoldProperties.Renderer`, and handle its `OnSetColor` or
`OnSetFont` event. `aFollower.Element` is the value of `BoldHandle`:

```pascal
procedure TMyForm.BoldAsStringRenderer1SetColor(aFollower: TBoldFollower; var AColor: TColor);
begin
  if Assigned(aFollower.Element) and
     not aFollower.Element.EvaluateExpressionAsBoolean('active') then
    AColor := clSilver;
end;
```

## When to Use TBoldLabel vs TBoldEdit

| Feature | TBoldLabel | TBoldEdit |
|---------|-----------|-----------|
| Editing | No | Yes |
| Weight | Lightweight (no window handle) | Heavier (window handle) |
| Best for | Display, derived values | User input fields |

## See Also

- [TBoldEdit](TBoldEdit.md) - Editable version
- [TBoldGrid](TBoldGrid.md) - Multi-row display
- [TBoldExpressionHandle](TBoldExpressionHandle.md) - Data source
