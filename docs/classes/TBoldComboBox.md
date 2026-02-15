# TBoldComboBox

`TBoldComboBox` is a data-aware combo box that combines a current value display with a dropdown list of selectable values. It connects to both an element handle (current value) and a list handle (available choices).

## Class Hierarchy

```mermaid
classDiagram
    TCustomComboBox <|-- TBoldCustomComboBox
    TBoldCustomComboBox <|-- TBoldComboBox

    TBoldComboBox : +BoldHandle (current value)
    TBoldComboBox : +BoldListHandle (choices)
    TBoldComboBox : +BoldRowProperties
    TBoldComboBox : +BoldSetValueExpression

    click TBoldListHandle href "../TBoldListHandle/" "TBoldListHandle"
    click TBoldExpressionHandle href "../TBoldExpressionHandle/" "TBoldExpressionHandle"
```

## Key Properties

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldElementHandle` | Current value handle |
| `BoldListHandle` | `TBoldAbstractListHandle` | Available choices |
| `BoldProperties` | `TBoldStringFollowerController` | Current value display |
| `BoldListProperties` | `TBoldComboListController` | List behavior |
| `BoldRowProperties` | `TBoldStringFollowerController` | How each row renders |
| `BoldSetValueExpression` | `string` | OCL expression for mapping selection |
| `BoldSelectChangeAction` | `TBoldComboSelectChangeAction` | What happens on select |
| `ReadOnly` | `Boolean` | Prevent selection |
| `AutoSearch` | `Boolean` | Type-ahead search in dropdown |

## Working with TBoldComboBox

### Selecting a Related Object

The most common use: selecting a related object from a list (e.g., choosing a Customer for an Order).

**Form Designer Setup:**

1. Drop `TBoldComboBox` on a form
2. Set `BoldHandle` to the current value (e.g., Order's customer reference)
3. Set `BoldListHandle` to a list of choices (e.g., all Customers)
4. Set `BoldRowProperties.Expression` for display text (e.g., `'name'`)
5. Set `BoldSetValueExpression` for how selection maps to value

### Example: Customer Selection

```
BoldComboBox1.BoldHandle → OrderExpressionHandle (expression: 'customer')
BoldComboBox1.BoldListHandle → CustomerListHandle (all customers)
BoldComboBox1.BoldRowProperties.Expression → 'name'
```

When the user selects a customer from the dropdown, Bold automatically sets the order's customer reference.

### Selection Change Event

```pascal
procedure TMyForm.ComboSelectChanged(Sender: TObject);
begin
  // React to selection change
  UpdateOrderDetails;
end;
```

### BoldListProperties Options

| Property | Description |
|----------|-------------|
| `NilElementMode` | How to handle nil (none, insert first, insert last) |
| `DragMode` | Drag behavior |
| `DropMode` | Drop behavior |

Setting `NilElementMode` to insert a nil element allows the user to clear the selection (choose "none").

## See Also

- [TBoldEdit](TBoldEdit.md) - Simple text editing
- [TBoldCheckBox](TBoldCheckBox.md) - Boolean selection
- [TBoldListHandle](TBoldListHandle.md) - Provides choices
- [TBoldGrid](TBoldGrid.md) - Grid with lookup columns
