# TBoldGrid

`TBoldGrid` is a grid that shows a Bold object list. It takes its rows from a list handle, usually a
`TBoldListHandle`, and each column shows the value of an OCL expression evaluated on the row's object.

!!! note "VCL only"
    `TBoldGrid` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show Bold
    data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

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
    TBoldGridColumn : +BoldProperties
    TBoldGridColumn : +Title
    TBoldGridColumn : +AllowCheckBox

    click TBoldListHandle href "../TBoldListHandle/" "TBoldListHandle"
```

## Key Properties

### TBoldGrid

| Property | Type | Description |
|----------|------|-------------|
| `BoldHandle` | `TBoldAbstractListHandle` | List handle providing the rows |
| `Columns` | `TBoldGridColumns` | Column collection |
| `BoldProperties` | `TBoldListAsFollowerListController` | List behavior control |
| `BoldAutoColumns` | `Boolean` | Create a column per attribute when the list type changes |
| `AddNewAtEnd` | `Boolean` | Down arrow on the last row adds a new object |
| `TitleFont` | `TFont` | Header font |
| `DefaultRowHeight` | `Integer` | Row height in pixels |

### TBoldGridColumn

| Property | Type | Description |
|----------|------|-------------|
| `BoldProperties` | `TBoldStringFollowerController` | Cell value and rendering; `BoldProperties.Expression` is the OCL expression |
| `Title.Caption` | `string` | Column header text |
| `Width` | `Integer` | Column width |
| `Alignment` | `TAlignment` | Text alignment |
| `AllowCheckBox` | `Boolean` | Render Boolean values as check boxes |
| `ColReadOnly` | `Boolean` | Prevent cell editing |
| `LookupHandle` | `TBoldAbstractListHandle` | Drop-down lookup list |

## Working with TBoldGrid

### Form Designer Setup

1. Drop a `TBoldGrid` on a form.
2. Set `BoldHandle` to a `TBoldListHandle`.
3. Double-click the grid to open the columns editor, or right-click it and choose **Create Default Columns** for a
   column per attribute.
4. Add columns and set each column's `BoldProperties.Expression`.

### Column Expressions

```text
name                            // Simple attribute
address.city                    // Navigate an association
orders->size                    // Collection count
salary.formatFloat('#,##0')     // Formatted display
```

### Creating Columns in Code

`Columns[0]` is the fixed indicator column the grid keeps for itself, and a new grid already has one empty data
column, `Columns[1]`. Do not call `Columns.Clear`: the grid re-creates column 0 at once and its column list gets
out of step. Reuse the existing columns and add more with `AddColumn`:

```pascal
procedure TForm1.SetupGrid;
var
  Col: TBoldGridColumn;
begin
  Col := BoldGrid1.Columns[1];
  Col.Title.Caption := 'Name';
  Col.BoldProperties.Expression := 'name';
  Col.Width := 150;

  Col := BoldGrid1.AddColumn;
  Col.Title.Caption := 'City';
  Col.BoldProperties.Expression := 'address.city';
  Col.Width := 100;

  Col := BoldGrid1.AddColumn;
  Col.Title.Caption := 'Active';
  Col.BoldProperties.Expression := 'active';
  Col.AllowCheckBox := True;
  Col.Width := 60;
end;
```

### Special Renderers

- **TBoldFirstColumnRenderer**: draws the row marker (triangle) in the first column
- **TBoldGridCheckBoxPainterRenderer**: draws Boolean values as check boxes
- **TBoldConstraintRenderer**: shows constraint violations (`BoldShowConstraints`)

## Common Patterns

### Sorting

Sort in the list handle's `Expression` with OCL `orderby`:

```pascal
BoldListHandle1.Expression := 'Customer.allInstances->orderby(name)';
```

### Conditional Cell Formatting

Put a `TBoldAsStringRenderer` on the form, assign it to the column's `BoldProperties.Renderer`, and handle its
`OnSetFont` or `OnSetColor` event. `aFollower.Element` is the row's object:

```pascal
procedure TMyForm.BoldAsStringRenderer1SetFont(aFollower: TBoldFollower; AFont: TFont);
begin
  if Assigned(aFollower.Element) and
     not aFollower.Element.EvaluateExpressionAsBoolean('active') then
    AFont.Color := clGray;
end;
```

## See Also

- [TBoldListHandle](TBoldListHandle.md) - Data source
- [TBoldEdit](TBoldEdit.md) - Single-value editing
- [TBoldNavigator](TBoldNavigator.md) - List navigation buttons
