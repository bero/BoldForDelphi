# TBoldGrid

`TBoldGrid` is a data-aware grid component that displays Bold object lists. It connects to a `TBoldListHandle` and shows objects as rows with configurable columns, each driven by an OCL expression.

## Class Hierarchy

```mermaid
classDiagram
    TCustomGrid <|-- TBoldCustomGrid
    TBoldCustomGrid <|-- TBoldGrid
    TBoldCustomGrid o-- TBoldGridColumns
    TBoldGridColumns o-- TBoldGridColumn

    TBoldGrid : +BoldHandle
    TBoldGrid : +Columns
    TBoldGrid : +AddNewAtEnd
    TBoldGridColumn : +Expression
    TBoldGridColumn : +Title
    TBoldGridColumn : +AllowCheckBox

    click TBoldListHandle href "../TBoldListHandle/" "TBoldListHandle"
```

## Key Properties

### TBoldGrid

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldAbstractListHandle` | List handle providing data |
| `Columns` | `TBoldGridColumns` | Column collection |
| `BoldProperties` | `TBoldListAsFollowerListController` | List behavior control |
| `AddNewAtEnd` | `Boolean` | New items at end of list |
| `TitleFont` | `TFont` | Header font |
| `DefaultRowHeight` | `Integer` | Row height in pixels |

### TBoldGridColumn

| Property | Type | Description |
|----------|------|-------------|
| `BoldProperties` | `TBoldStringFollowerController` | Cell rendering control |
| `Expression` | `string` | OCL expression for cell value |
| `Title.Caption` | `string` | Column header text |
| `Width` | `Integer` | Column width |
| `Alignment` | `TAlignment` | Text alignment |
| `AllowCheckBox` | `Boolean` | Render boolean as checkbox |
| `ReadOnly` | `Boolean` | Prevent cell editing |
| `LookupHandle` | `TBoldAbstractListHandle` | Dropdown lookup list |

## Working with TBoldGrid

### Form Designer Setup

1. Drop `TBoldGrid` on a form
2. Set `BoldHandle` to a `TBoldListHandle`
3. Open the Columns editor (double-click or right-click)
4. Add columns and set each column's `BoldProperties.Expression`

### Column Expressions

```
name                          // Simple attribute
address.city                  // Navigate association
orders->size                  // Collection count
salary.formatFloat('#,##0')  // Formatted display
```

### Programmatic Column Creation

```pascal
procedure SetupGrid;
var
  Col: TBoldGridColumn;
begin
  BoldGrid1.Columns.Clear;

  Col := BoldGrid1.Columns.Add;
  Col.Title.Caption := 'Name';
  Col.BoldProperties.Expression := 'name';
  Col.Width := 150;

  Col := BoldGrid1.Columns.Add;
  Col.Title.Caption := 'City';
  Col.BoldProperties.Expression := 'address.city';
  Col.Width := 100;

  Col := BoldGrid1.Columns.Add;
  Col.Title.Caption := 'Active';
  Col.BoldProperties.Expression := 'active';
  Col.AllowCheckBox := True;
  Col.Width := 60;
end;
```

### Special Renderers

- **TBoldFirstColumnRenderer** — draws a row marker (triangle) in the first column
- **TBoldGridCheckBoxPainterRenderer** — renders boolean values as checkboxes
- **TBoldConstraintRenderer** — highlights constraint violations

## Common Patterns

### Sorting by Column

Configure sorting via the list handle's `Expression` with OCL `orderby`:

```pascal
BoldListHandle1.Expression := 'Customer.allInstances->orderby(name)';
```

### Conditional Cell Formatting

Use a custom renderer to change font/color based on values:

```pascal
// In BoldProperties.OnGetFont or via a TBoldAsStringRenderer subclass
procedure TMyForm.GridGetFont(...)
begin
  if Element.EvaluateExpressionAsString('active') = 'False' then
    aFont.Color := clGray;
end;
```

## See Also

- [TBoldListHandle](TBoldListHandle.md) - Data source
- [TBoldEdit](TBoldEdit.md) - Single-value editing
- [TBoldNavigator](TBoldNavigator.md) - List navigation buttons
