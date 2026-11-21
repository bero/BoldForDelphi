# Bold Data Set and Reporting

`TBoldDataSet` presents a Bold list as an ordinary `TDataSet`. Anything that
consumes a dataset, a report writer above all, can then consume Bold objects
without knowing that Bold exists.

This demo pairs a small master-detail form over `Person` and `Building` with a
QuickReport that prints the buildings and their residents through two of those
datasets.

## Running it

There is no `.dproj`, only `BE_DataSetAndReporting.dpr`, a VCL program for
Windows. Open it in the IDE and let Delphi create a project file.

**The program needs QuickReport**, which has not shipped with Delphi for a long
time. `FfrmReport.pas` uses `QuickRpt` and `Qrctrls`, and `fMain` uses
`FfrmReport` for its report button, so the program does not compile without it.

It needs a database: InterBase through `TBoldDatabaseAdapterIB` on the data
module, `sysdba` / `masterkey`, and **`IBDatabase1.DatabaseName` is empty**, so
set it first. The Bold IBX units, `BoldDatabaseAdapterIB` and
`BoldIBDatabaseAction`, live under `Source\Deprecated\Persistance\IBX` and are
in no current Bold package, so put that folder on the unit search path.

Startup runs `TfrmStart` modally before anything else. It has three buttons:
**Create schema** (`BoldIBDatabaseAction1`), **Open system**
(`BoldActivateSystemAction1`, `ModalResult` 1) and **Cancel**. The main form is
created only if the dialog returns `idOK`, so pressing Cancel exits.

## The one idea: OCL columns in a dataset

`TBoldDataSet` has a `FieldDescriptions` collection. Each item pairs an OCL
expression with a field name, and that is the whole mapping:

| Dataset | `BoldProperties.Expression` | `FieldName` |
| --- | --- | --- |
| `bdsBuilding` | `zipCode` | `Zip Code` |
| `bdsBuilding` | `address` | `Address` |
| `bdsResident` | `firstName` | `First Name` |
| `bdsResident` | `lastName` | `Last Name` |

Field names can contain spaces because they are dataset field names, not model
identifiers. The report's `TQRDBText` controls bind to them by name, exactly as
they would to a table.

Each dataset takes its rows from a handle, not from a query:

```
blhBuildings : TBoldListHandle  root = dmMain.bshMain  Expression = 'Building.allInstances'
blhResidents : TBoldListHandle  root = blhBuildings    Expression = 'residents'
```

That is where the master-detail in the report comes from. `bndBuildings` prints
from `bdsBuilding` and `bndResidents` from `bdsResident`; because
`blhResidents` is rooted on `blhBuildings`, the detail follows the master
cursor by itself. No QuickReport master-detail link is configured.

Both datasets have `AutoOpen = False`, so `btnGenerateReportClick` opens them
before previewing:

```pascal
with TfrmReport.Create(Self) do
begin
  bdsBuilding.Active := True;
  bdsResident.Active := True;
  qrMain.Preview;
end;
```

## The main form

The same kind of handle chain drives the screen: `blhPersons`, `blhBuildings`,
`blhPersonHome` (a `TBoldExpressionHandle` over `home`) and `blhResidents`. The
list box renders residents with `firstName + ' ' + lastName`.

**Debug, SystemDebugger** opens `TBoldSystemDebuggerFrm` on the live system,
which is worth knowing about on its own: it shows every object, every member
and its dirty state.

## Files

| File | What it holds |
| --- | --- |
| `BE_DataSetAndReporting.dpr` | data module, then the start dialog, then the main form |
| `fStart.pas` / `.dfm` | create schema / open system / cancel |
| `dMain.pas` / `.dfm` | system handle, model, persistence, the Bold actions |
| `fMain.pas` / `.dfm` | master-detail screen and the system debugger menu |
| `FfrmReport.pas` / `.dfm` | the two `TBoldDataSet`s and the QuickReport |
| `DataSetExampleClasses*.pas/.inc` | generated `Person` and `Building` |
