# TBoldLabel

`TBoldLabel` is a data-aware label that displays a Bold attribute or OCL expression result as read-only text. It automatically updates when the underlying data changes.

## Class Hierarchy

```mermaid
classDiagram
    TCustomLabel <|-- TBoldCustomLabel
    TBoldCustomLabel <|-- TBoldLabel

    TBoldLabel : +BoldHandle
    TBoldLabel : +BoldProperties
    TBoldLabel : +Expression
    TBoldLabel : +Caption (auto-set)

    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Handle providing the value |
| `BoldProperties` | `TBoldStringFollowerController` | Controls rendering |
| `Expression` | `string` | OCL expression for the displayed value |
| `Color` | `TColor` | Background color |
| `Font` | `TFont` | Text font |

| Public Property | Description |
|-----------------|-------------|
| `Caption` | Automatically set from the Bold value |
| `EffectiveFont` | Font after renderer override |
| `EffectiveColor` | Color after renderer override |

## Working with TBoldLabel

### Form Designer Setup

1. Drop `TBoldLabel` on a form
2. Set `BoldHandle` to an expression handle
3. Optionally set `BoldProperties.Expression` for sub-navigation
4. The label text updates automatically when data changes

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

Use a custom renderer to change appearance based on values:

```pascal
// Via BoldProperties.OnGetColor
procedure TMyForm.LabelGetColor(...)
begin
  if Element.EvaluateExpressionAsString('active') = 'False' then
    aColor := clSilver;
end;
```

## When to Use TBoldLabel vs TBoldEdit

| Feature | TBoldLabel | TBoldEdit |
|---------|-----------|-----------|
| Editing | No | Yes |
| Weight | Lightweight | Heavier (window handle) |
| Best for | Display, derived values | User input fields |

## See Also

- [TBoldEdit](TBoldEdit.md) - Editable version
- [TBoldGrid](TBoldGrid.md) - Multi-row display
- [TBoldExpressionHandle](TBoldExpressionHandle.md) - Data source
