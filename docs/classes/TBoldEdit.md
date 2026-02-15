# TBoldEdit

`TBoldEdit` is a data-aware edit control that displays and edits a single Bold attribute value. It connects to a `TBoldElementHandle` or `TBoldExpressionHandle` and provides automatic validation and rendering.

## Class Hierarchy

```mermaid
classDiagram
    TCustomEdit <|-- TBoldCustomEdit
    TBoldCustomEdit <|-- TBoldEdit

    TBoldEdit : +BoldHandle
    TBoldEdit : +BoldProperties
    TBoldEdit : +Expression
    TBoldEdit : +ReadOnly
    TBoldEdit : +ButtonStyle

    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Handle providing the value |
| `BoldProperties` | `TBoldStringFollowerController` | Controls rendering and validation |
| `Expression` | `string` | OCL expression for sub-navigation |
| `ReadOnly` | `Boolean` | Prevent editing |
| `ButtonStyle` | `TBoldEditButtonStyle` | None, ellipsis, or dropdown button |
| `Alignment` | `TAlignment` | Text alignment |
| `BeepOnInvalidKey` | `Boolean` | Audio feedback on invalid input |

| Public Property | Description |
|-----------------|-------------|
| `EffectiveReadOnly` | Combines ReadOnly + model constraints |
| `EffectiveFont` | Font after renderer override |
| `EffectiveColor` | Color after renderer override |
| `Follower` | The internal follower tracking the value |

## Working with TBoldEdit

### Form Designer Setup

1. Drop `TBoldEdit` on a form
2. Set `BoldHandle` to an expression handle
3. The edit automatically displays and allows editing the value

### Connecting to Attributes

```pascal
// Expression handle connected to current customer's name
ExpressionHandle1.RootHandle := BoldSystemHandle1;
ExpressionHandle1.Expression := 'Customer.allInstances->first';

BoldEdit1.BoldHandle := ExpressionHandle1;
BoldEdit1.BoldProperties.Expression := 'name';
```

### Validation

`TBoldEdit` validates input automatically:

- **Per-keystroke**: `ValidateCharacter` prevents invalid characters
- **On exit**: `ValidateString` checks the complete value
- Invalid keys trigger a beep if `BeepOnInvalidKey` is `True`

### Button Style

```pascal
// Add an ellipsis button for custom editor
BoldEdit1.ButtonStyle := besEllipsis;
BoldEdit1.OnButtonClick := HandleButtonClick;

procedure TMyForm.HandleButtonClick(Sender: TObject);
begin
  // Open custom editor dialog
  ShowAddressEditor(CurrentCustomer);
end;
```

## See Also

- [TBoldLabel](TBoldLabel.md) - Read-only display
- [TBoldComboBox](TBoldComboBox.md) - Selection from list
- [TBoldExpressionHandle](TBoldExpressionHandle.md) - Data source
