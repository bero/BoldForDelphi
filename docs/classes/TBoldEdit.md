# TBoldEdit

`TBoldEdit` is an edit box that shows and edits a single Bold value. It takes the value from an element handle,
usually a `TBoldExpressionHandle`, and validates what the user types.

!!! note "VCL only"
    `TBoldEdit` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show Bold
    data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

## Class Hierarchy

```mermaid
classDiagram
    TCustomEdit <|-- TBoldCustomEdit
    TBoldCustomEdit <|-- TBoldEdit

    TBoldEdit : +BoldHandle
    TBoldEdit : +BoldProperties
    TBoldEdit : +ReadOnly
    TBoldEdit : +ButtonStyle

    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Handle providing the value |
| `BoldProperties` | `TBoldStringFollowerController` | Rendering and validation; `BoldProperties.Expression` navigates from the handle's value |
| `ReadOnly` | `Boolean` | Prevent editing |
| `ButtonStyle` | `TBoldEditButtonStyle` | `bbsNone`, `bbsCombo`, `bbsEllipsis` or `bbsCustom` |
| `OnButtonClick` | `TNotifyEvent` | Fires when the button is clicked |
| `Alignment` | `TAlignment` | Text alignment |

| Public Property | Description |
|-----------------|-------------|
| `EffectiveReadOnly` | `True` when `ReadOnly` is set or the value cannot be modified |
| `EffectiveFont` | Font after renderer override |
| `EffectiveColor` | Color after renderer override |
| `Button` | The speed button shown for `ButtonStyle` |

## Working with TBoldEdit

### Form Designer Setup

1. Drop a `TBoldEdit` on a form.
2. Set `BoldHandle` to an expression handle.
3. Optionally set `BoldProperties.Expression` to navigate from the handle's value.

### Connecting to Attributes

```pascal
// Expression handle on the first customer
ExpressionHandle1.RootHandle := BoldSystemHandle1;
ExpressionHandle1.Expression := 'Customer.allInstances->first';

// The edit shows and edits the customer's name
BoldEdit1.BoldHandle := ExpressionHandle1;
BoldEdit1.BoldProperties.Expression := 'name';
```

### Validation

`TBoldEdit` validates input through the renderer of `BoldProperties`:

- **Per keystroke**: characters the renderer's `ValidateCharacter` rejects are dropped, with a beep.
- **When applied**: the whole string is checked with `ValidateString` before it is written to the value. By default
  the value is applied when the edit loses focus (`BoldProperties.ApplyPolicy = bapExit`).
- **Esc** discards the change.

### Button Style

```pascal
// Add an ellipsis button that opens a custom editor
BoldEdit1.ButtonStyle := bbsEllipsis;
BoldEdit1.OnButtonClick := HandleButtonClick;

procedure TMyForm.HandleButtonClick(Sender: TObject);
begin
  ShowAddressEditor(CurrentCustomer);
end;
```

## See Also

- [TBoldLabel](TBoldLabel.md) - Read-only display
- [TBoldComboBox](TBoldComboBox.md) - Selection from a list
- [TBoldExpressionHandle](TBoldExpressionHandle.md) - Data source
