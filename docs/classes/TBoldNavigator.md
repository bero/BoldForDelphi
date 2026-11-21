# TBoldNavigator

`TBoldNavigator` is a button bar for a Bold list: move through the list, insert, delete and reorder objects. It
connects to a list handle, usually a `TBoldListHandle`, and enables or disables its buttons from the cursor
position and what the list allows.

!!! note "VCL only"
    `TBoldNavigator` is a VCL control for Delphi on Windows. FireMonkey (FMX) and Lazarus (LCL) applications show
    Bold data through `TBoldDataSet`, with LiveBindings in FMX and the LCL's data-aware controls; see the Invoicing
    example ([FMX](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/FMX/README.md),
    [LCL](https://github.com/bero/BoldForDelphi/blob/develop/examples/Compound/Invoicing/LCL/README.md)).

## Class Hierarchy

```mermaid
classDiagram
    TCustomPanel <|-- TBoldNavigateBtnImageIndexOwner
    TBoldNavigateBtnImageIndexOwner <|-- TBoldCustomNavigator
    TBoldCustomNavigator <|-- TBoldNavigator

    TBoldNavigator : +BoldHandle
    TBoldNavigator : +VisibleButtons
    TBoldNavigator : +ConfirmDelete
    TBoldNavigator : +BoldDeleteMode

    click TBoldListHandle href "../TBoldListHandle/" "TBoldListHandle"
```

## Key Properties

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `BoldHandle` | `TBoldAbstractListHandle` | nil | List to navigate |
| `VisibleButtons` | `TBoldButtonSet` | First, Prior, Next, Last, Insert, Delete | Visible buttons |
| `ConfirmDelete` | `Boolean` | True | Ask before delete, unlink or remove |
| `BoldDeleteMode` | `TBoldDeleteMode` | dmDefault | What the delete button does |
| `Flat` | `Boolean` | False | Flat button style |
| `Hints` | `TStrings` | | Tooltip text per button |
| `DeleteQuestion` | `string` | | Custom delete confirmation text |
| `UnlinkQuestion` | `string` | | Custom unlink confirmation text |
| `RemoveQuestion` | `string` | | Custom remove-from-list confirmation text |
| `Images` | `TImageList` | | Custom button images |
| `ImageIndices` | `TBoldNavigateBtnImageIndex` | | Custom image indices |
| `OnClick` | `TBoldNavClick` | | Fires after a button has done its action |

## Navigation Buttons

```pascal
TBoldNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
                    nbInsert, nbDelete, nbMoveUp, nbMoveDown);
```

| Button | Action | Disabled when |
|--------|--------|---------------|
| `nbFirst` | Go to the first element | Already at the first |
| `nbPrior` | Go to the previous element | Already at the first |
| `nbNext` | Go to the next element | Already at the last |
| `nbLast` | Go to the last element | Already at the last |
| `nbInsert` | Create a new object in the list | The list is not mutable or cannot create objects |
| `nbDelete` | Delete or remove the current object | No current object |
| `nbMoveUp` | Move the current element up | The list cannot move it |
| `nbMoveDown` | Move the current element down | The list cannot move it |

## Delete Modes

| Mode | Behavior |
|------|----------|
| `dmDefault` | Deletes the object when the list is a class list (`allInstances`), otherwise works as `dmRemoveFromList` |
| `dmRemoveFromList` | Removes the object from the list: unlinks it for an association, deletes the link object for a link role |
| `dmDelete` | Deletes the object; the button is disabled when the object cannot be deleted |
| `dmUnlinkAllAndDelete` | Removes all the object's links, then deletes it |

`dmDelete` and `dmUnlinkAllAndDelete` run in a transaction.

## Working with TBoldNavigator

### Form Designer Setup

1. Drop a `TBoldNavigator` on a form.
2. Set `BoldHandle` to the same `TBoldListHandle` as your grid.
3. Choose the buttons in `VisibleButtons`.
4. Leave `ConfirmDelete` set to `True` for safety.

### Typical Layout

```text
[TBoldNavigator] <-> [TBoldListHandle] <-> [TBoldGrid]
     buttons            data source         display
```

### Custom Button Visibility

```pascal
// Navigation buttons only (no insert or delete)
BoldNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];

// All buttons, including reorder
BoldNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast,
  nbInsert, nbDelete, nbMoveUp, nbMoveDown];
```

### Navigating in Code

```pascal
// Navigate through the list handle
BoldListHandle1.First;
BoldListHandle1.Next;
BoldListHandle1.Last;
```

## See Also

- [TBoldGrid](TBoldGrid.md) - Grid display
- [TBoldListHandle](TBoldListHandle.md) - Data source
- [TBoldEdit](TBoldEdit.md) - Single-value editing
