# TBoldComboBox

`TBoldComboBox` is a combo box that shows a current value and lets the user pick a new one from a drop-down list.
It connects to two handles: an element handle for the current value and a list handle for the choices. The usual
use is to pick a related object, such as the customer of an order.

!!! note "VCL only"
    `TBoldComboBox` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show Bold
    data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

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
| `BoldHandle` | `TBoldElementHandle` | Handle of the element that holds the current value |
| `BoldListHandle` | `TBoldAbstractListHandle` | Available choices |
| `BoldProperties` | `TBoldStringFollowerController` | Display of the current value |
| `BoldListProperties` | `TBoldComboListController` | List behavior |
| `BoldRowProperties` | `TBoldStringFollowerController` | Display of each row in the drop-down list |
| `BoldSetValueExpression` | `string` | OCL expression, relative to the value of `BoldHandle`, for the element the selection is assigned to |
| `BoldSelectChangeAction` | `TBoldComboSelectChangeAction` | What a selection does (see below) |
| `OnSelectChanged` | `TNotifyEvent` | Fires after a selection has been handled |
| `ReadOnly` | `Boolean` | Prevent selection |

### Select Change Actions

| Value | What a selection does |
|-------|-----------------------|
| `bdscSetText` | Sets the text, which is then applied to the current value as a string (the default) |
| `bdcsSetValue` | Evaluates `BoldSetValueExpression` on the value of `BoldHandle` (or takes that value when the expression is empty); an object reference gets the selected object, other elements get its string value |
| `bdcsSetReference` | Makes the selected element the value of `BoldHandle`; needs a handle that accepts a value, such as `TBoldReferenceHandle` |
| `bdcsSetListIndex` | Makes the selected element current in `BoldListHandle` |
| `bdcsNone` | Nothing |

## Working with TBoldComboBox

### Selecting a Related Object

To choose the customer of an order:

1. Drop a `TBoldComboBox` on a form.
2. Set `BoldHandle` to a handle whose value is the order.
3. Set `BoldListHandle` to a list of all customers.
4. Set `BoldProperties.Expression` to `customer.name` and `BoldRowProperties.Expression` to `name`.
5. Set `BoldSetValueExpression` to `customer` and `BoldSelectChangeAction` to `bdcsSetValue`.

```text
BoldComboBox1.BoldHandle                  -> OrderHandle (value: the order)
BoldComboBox1.BoldListHandle              -> CustomerListHandle (Customer.allInstances)
BoldComboBox1.BoldProperties.Expression    = 'customer.name'
BoldComboBox1.BoldRowProperties.Expression = 'name'
BoldComboBox1.BoldSetValueExpression       = 'customer'
BoldComboBox1.BoldSelectChangeAction       = bdcsSetValue
```

When the user selects a customer, the combo box sets the order's `customer` reference. The account combo boxes in
`examples/Simple/ObjectSpace/Transaction` are set up this way.

### Selection Change Event

`OnSelectChanged` fires after the select change action has run:

```pascal
procedure TMyForm.BoldComboBox1SelectChanged(Sender: TObject);
begin
  UpdateOrderDetails;
end;
```

### BoldListProperties Options

| Property | Description |
|----------|-------------|
| `NilElementMode` | Extra empty row: `neNone`, `neInsertFirst` or `neAddLast` |
| `DragMode` | Drag behavior |
| `DropMode` | Drop behavior |

With `NilElementMode` set to `neInsertFirst` or `neAddLast`, the user can clear the selection by choosing the empty
row.

## See Also

- [TBoldEdit](TBoldEdit.md) - Simple text editing
- [TBoldCheckBox](TBoldCheckBox.md) - Boolean selection
- [TBoldListHandle](TBoldListHandle.md) - Provides the choices
- [TBoldGrid](TBoldGrid.md) - Grid with lookup columns
