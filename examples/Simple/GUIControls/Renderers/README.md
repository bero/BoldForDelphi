# Renderers

A renderer component sits between a Bold control and the object it follows and
takes over any of four jobs: producing the string, accepting one back, deciding
whether editing is allowed, and deciding what to subscribe to. Nothing about the
model changes; the grid simply asks the renderer instead of the attribute.

The two renderers on this form split those jobs differently, which is the point
of putting them side by side.

## Running it

There is no `.dproj`, only `BE_Renderers.dpr`. Open it in Delphi and let the IDE
create a project file. Persistence is IBX (`TBoldDatabaseAdapterIB`,
`TBoldIBDatabaseAction`, `TIBDatabase`), whose units live under
`Source\Deprecated\Persistance\IBX` and are in no current Bold package, so put
that folder on the unit search path.

`IBDatabase1` has credentials but **no `DatabaseName`** - set it, then press
**Create DB** and **Open system**. Add people with the navigator;
`TPerson.CompleteCreate` in `RendererExampleClasses.inc` fills in a random name
and a salary between 3000 and 13000. Nothing calls `Randomize`, so each run
produces the same sequence.

## The model

| Class | Attributes |
| --- | --- |
| `Person` | `FirstName`, `LastName` (String), `Salary` (Currency) |
| `Globals` | `SalaryBreakPoint` (Currency) |

`Globals` is a singleton by convention, not by constraint. On system open the
form takes the first instance or creates one, and puts it in `brhGlobals`, a
`TBoldReferenceHandle` with `StaticValueTypeName = 'Globals'`.

## The grid

`BoldGrid1` follows `blhPerson` (`Person.allInstances`). Its five columns:

| Column | BoldProperties.Expression | BoldProperties.Renderer |
| --- | --- | --- |
| row indicator | | |
| First Name | `firstName` | |
| Last Name | `lastName` | |
| Full Name | *(none)* | `bsrFullName` |
| Salary | `salary` | `bsrSalaryLevel` |

### bsrFullName: a column with no expression

The Full Name column has a renderer and no expression at all, so the renderer is
the only source of its content. `bsrFullName` is a `TBoldAsStringRenderer` with
four handlers:

- `OnGetAsString` joins `FirstName` and `LastName`, substituting `<FirstName>`
  or `<LastName>` when either is blank.
- `OnSetAsString` splits what you typed at the first space and writes both
  attributes back, so the column is editable in both directions.
- `OnSubscribe` calls `SubscribeToExpression('firstName', ...)` and the same for
  `lastName`. This is the handler that is easy to forget: without it the column
  would not repaint when you edit either of the first two columns.
- `OnMayModify` asks both members through `ObserverMayModify`, so the cell goes
  read-only whenever either underlying attribute is.

### bsrSalaryLevel: rendering that only paints

The Salary column keeps its expression and adds `bsrSalaryLevel`, which
implements only `OnSetColor` and `OnSubscribe`. The value still comes from the
attribute; the renderer just turns the cell `clSilver` when

```pascal
(element as TPerson).Salary < (brhGlobals.value as TGlobals).SalaryBreakPoint
```

Its `OnSubscribe` is the part worth reading. It subscribes to `salary` on the
element, as expected, and then also to

```
Globals.allInstances->first.salaryBreakPoint
```

on the system. A colour that depends on two things has to subscribe to both, and
the second one is not reachable from the element being rendered.

## Watching it react

Two `TBoldTrackBar` controls, both with `BoldProperties.ApplyPolicy = bapChange`
so they write on every move:

| Control | BoldHandle | Expression |
| --- | --- | --- |
| `BoldTrackBar1` | `blhPerson` | `salary` |
| `BoldTrackBar2` | `brhGlobals` | `salaryBreakPoint` |

Drag the lower one and the whole Salary column recolours as the break point
passes each row. `BoldLabel1` shows the same `salaryBreakPoint` as text.
