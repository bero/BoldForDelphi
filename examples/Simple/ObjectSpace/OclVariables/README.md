# OCL Variables

An OCL expression in a DFM is evaluated against one object, so on its own it can
only see that object and what it can navigate to. `TBoldOclVariables` widens
that: it binds a name to a handle, and any expression given the variable list
can use the name.

This demo prices products with a VAT rate that is stored on a different object
altogether.

## Running it

There is no `.dproj`. Open `BE_OclVariables.dpr` in the IDE and let Delphi
create one.

It needs a database: InterBase through `TBoldDatabaseAdapterIB`, `sysdba` /
`masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so set it first. Then
**Create DB** and **Open system**, add a product with the navigator, and edit
the VAT box. The label says what to watch: *Change this value and see the value
'Retail Price' recalculate.*

`MainForm.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (8867 bytes against 8825 in the
object store), so it will not stream until you restore it with
`git show HEAD:<path> > <path>`.

## The one idea

The third column of `BoldGrid1` is

```
net_Price + (net_Price * global_VAT / 100)
```

with `BoldProperties.Variables` set to the `BoldOclVariables` component. The
grid is rooted on `blhProduct` (`Product.allInstances`), so `net_Price` reads
from the row's product. `global_VAT` comes from elsewhere:

| Component | Setting |
| --- | --- |
| `BoldOclVariables` | one item: `VariableName = 'global_VAT'`, `BoldHandle = behVAT`, `UseListElement = False` |
| `behVAT` | `TBoldExpressionHandle`, root `BoldSystemHandle1`, `GlobalSettings.allInstances->first.vat` |
| `btxtVAT` | `TBoldEdit` bound to `behVAT` |

The variable is bound to the **handle**, not to a value read once. When you
type in the VAT edit, the handle's value changes, the grid column is subscribed
to it through the variable, and every Retail Price cell repaints. There is no
event handler.

`UseListElement` matters when the handle is a list: with it set, the variable
resolves to the current element rather than the whole list. Here `behVAT` is
already single-valued, so it is off.

## Keeping exactly one GlobalSettings

The expression `GlobalSettings.allInstances->first.vat` needs an instance to
exist. `BoldSystemActivator1SystemActivated`, wired to the activate action's
`OnSystemOpened`, creates one on a fresh database and saves it immediately:

```pascal
if BoldSystemHandle1.System.ClassByExpressionName['GlobalSettings'].Count = 0 then
begin
  TGlobalSettings.Create(BoldSystemHandle1.System);
  BoldSystemHandle1.UpdateDatabase;
end;
```

## The same problem solved differently

`DerivedAttributes` next door produces the same retail price from a derived
model attribute, with `global_VAT` registered through
`System.Evaluator.DefineVariable` instead of a DFM component. The models are
almost identical, which makes the pair a fair comparison. A variable list is
local to the expressions that reference it; a derived attribute is part of the
model and visible to everything.

## Files

| File | What it holds |
| --- | --- |
| `BE_OclVariables.dpr` | creates `TForm1` and runs |
| `MainForm.pas` / `.dfm` | the variable list, the handles, the embedded model |
| `OclVariableClasses*.pas/.inc` | generated `Product` and `GlobalSettings` |
