# TBoldNavigator

`TBoldNavigator` provides navigation and CRUD buttons for Bold object lists. It connects to a `TBoldListHandle` and automatically enables/disables buttons based on cursor position and list mutability.

## Class Hierarchy

```mermaid
classDiagram
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
| `VisibleButtons` | `TBoldButtonSet` | First,Prior,Next,Last,Insert,Delete | Visible buttons |
| `ConfirmDelete` | `Boolean` | True | Show delete confirmation |
| `BoldDeleteMode` | `TBoldDeleteMode` | dmDefault | How to remove objects |
| `Flat` | `Boolean` | False | Flat button style |
| `Hints` | `TStrings` | | Tooltip text per button |
| `DeleteQuestion` | `string` | | Custom delete confirmation text |
| `UnlinkQuestion` | `string` | | Custom unlink confirmation text |
| `RemoveQuestion` | `string` | | Custom remove-from-list text |
| `Images` | `TImageList` | | Custom button images |
| `ImageIndices` | `TBoldNavigateBtnImageIndex` | | Custom image indices |

## Navigation Buttons

```
TBoldNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast,
                    nbInsert, nbDelete, nbMoveUp, nbMoveDown);
```

| Button | Action | Auto-disabled when |
|--------|--------|--------------------|
| `nbFirst` | Go to first element | Already at first |
| `nbPrior` | Go to previous | Already at first |
| `nbNext` | Go to next | Already at last |
| `nbLast` | Go to last element | Already at last |
| `nbInsert` | Create new object | List not mutable |
| `nbDelete` | Delete/remove current | List not mutable or empty |
| `nbMoveUp` | Reorder: move up | At first position |
| `nbMoveDown` | Reorder: move down | At last position |

## Delete Modes

| Mode | Behavior |
|------|----------|
| `dmDefault` | Deletes the object from the system |
| `dmRemoveFromList` | Removes from list without deleting |
| `dmUnlinkObject` | Unlinks association without deleting |

## Working with TBoldNavigator

### Form Designer Setup

1. Drop `TBoldNavigator` on a form
2. Set `BoldHandle` to the same `TBoldListHandle` as your grid
3. Configure `VisibleButtons` for the desired actions
4. Set `ConfirmDelete := True` for safety

### Typical Layout

```
[TBoldNavigator] ←→ [TBoldListHandle] ←→ [TBoldGrid]
     buttons           data source          display
```

### Custom Button Visibility

```pascal
// Show only navigation buttons (no CRUD)
BoldNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];

// Show all including reorder
BoldNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast,
  nbInsert, nbDelete, nbMoveUp, nbMoveDown];
```

### Programmatic Navigation

```pascal
// Navigate programmatically via the list handle
BoldListHandle1.First;
BoldListHandle1.Next;
BoldListHandle1.Last;
```

## See Also

- [TBoldGrid](TBoldGrid.md) - Grid display
- [TBoldListHandle](TBoldListHandle.md) - Data source
- [TBoldEdit](TBoldEdit.md) - Single-value editing
