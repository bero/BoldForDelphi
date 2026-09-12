# Default Bold Project

Not a demo of a feature, but the skeleton a new Bold application used to start
from: the five persistence components wired together, an empty model waiting to
be replaced, and a main form that contains its own instructions.

It is worth reading for one reason. Every other example in this repository
arrives with its model, its handles and its database already set up, so it never
shows you what the minimum is. This does.

## Running it

There is no `.dproj`, only `BoldProject.dpr`. Open it in Delphi and let the IDE
create a project file. Persistence is IBX (`TBoldDatabaseAdapterIB`,
`TBoldIBDatabaseAction`, `TIBDatabase`), whose units live under
`Source\Deprecated\Persistance\IBX` and are in no current Bold package, so put
that folder on the unit search path. `IBDatabase1` has
`user_name=sysdba` / `password=masterkey` and **no `DatabaseName`**.

Running it as it stands gets you a working application with nothing in it: the
model is called `EmptyModel` and contains a single class, `RootClass`, with no
attributes. The instructions in `fMain` tell you the next steps - import a model
through `BoldUMLRoseLink1` or another model link, open the model editor by
double-clicking `bmoMain`, generate code, then press **Create DB**.

## The startup sequence

`BoldProject.dpr` does something no other example here does. It creates the data
module, then shows a dialog **before** the main form:

```pascal
Application.CreateForm(TdmMain, dmMain);
with TfrmStart.Create(application) do
try
  if ShowModal = idOK then
  begin
    Application.CreateForm(TfrmMain, frmMain);
    Application.Run;
  end;
finally
  Free;
end;
```

`TfrmStart` is three buttons and no code at all. Two of them borrow actions from
the data module, and the modal results do the rest:

| Button | Action | ModalResult |
| --- | --- | --- |
| `btnCreateSchema` | `dmMain.BoldIBDatabaseAction1` | none |
| `btnOpenSystem` | `dmMain.BoldActivateSystemAction1` | 1 (`mrOk`) |
| `btnCancel` | | 2 (`mrCancel`) |

So the main form is only ever created after the system has been opened, and
cancelling ends the program without one. That sidesteps the usual awkwardness of
a main form whose controls have no live system behind them.

## The data module

`dMain` holds the chain every Bold application needs, and nothing else:

| Component | Class | Points at |
| --- | --- | --- |
| `bmoMain` | `TBoldModel` | the model, stored inline in the DFM |
| `stiMain` | `TBoldSystemTypeInfoHandle` | `bmoMain` |
| `bshMain` | `TBoldSystemHandle` | `stiMain` + `BoldPersistenceHandleDB1`, `IsDefault = True` |
| `BoldPersistenceHandleDB1` | `TBoldPersistenceHandleDB` | `BoldDatabaseAdapterIB1` |
| `BoldDatabaseAdapterIB1` | `TBoldDatabaseAdapterIB` | `IBDatabase1` |
| `BoldUMLRoseLink1` | `TBoldUMLRoseLink` | `bmoMain`, for importing a model |

`ActionList1` carries the three standard actions: `BoldActivateSystemAction1`
(`SaveOnClose = saAsk`), `BoldUpdateDBAction1` and `BoldIBDatabaseAction1`.

## The main form

`fMain` is a read-only memo holding the instructions, plus a menu: **File,
Update database** is the action; **Debug, SystemDebugger** opens

```pascal
TBoldSystemDebuggerFrm.CreateWithSystem(self, dmMain.bshMain.System).show;
```

which is the point of the template - before you have built any UI, the system
debugger already lets you browse classes, list instances and create objects.
`FormCloseQuery` offers to discard when `BoldDirty`.

## Two stale leftovers

`dMain.pas` still declares `BoldIBAliasAction1SchemaGenerated`, which reopens the
system after schema generation, but nothing in `dMain.dfm` assigns it to
`BoldIBDatabaseAction1.OnSchemaGenerated`, so it never runs.

`dMain.dti`, the data-module diagram, is annotated with explanations of each
component and is the nicest part of the template to look at - but it still names
`bphBDEMain`, a BDE persistence handle that no longer exists, and
`BoldIBAliasAction1` under its old name.
