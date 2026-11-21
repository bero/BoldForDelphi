# First Application

This guide builds a small Bold application from an empty Delphi VCL project: **persons** and the **buildings** they
own, stored in SQLite. It takes about half an hour and shows every piece a Bold application needs.

!!! tip "Other routes"
    - To see a finished application first, run the [Quick Start](../quickstart.md) example.
    - For a longer guided course with explanations, follow the
      [Tutorial](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial), which builds an invoicing
      application in five steps.
    - For FireMonkey or Lazarus, start from the Invoicing example's
      [FMX](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/FMX) or
      [LCL](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/LCL) client: those
      frameworks bind through `TBoldDataSet` instead of the VCL controls used here.

Before you start, [install](installation.md) the Bold package in Delphi.

## Step 1: Create the project

1. **File → New → Windows VCL Application** and save the project, for example as `Building.dproj`.
2. Put Bold's source folders on the project's search path, as
   [Installation, Step 3](installation.md#step-3-let-your-projects-find-bolds-source) describes. Skip this if you
   installed Bold with a package manager.

## Step 2: Add a data module

**File → New → Other → Delphi Files → Data Module**, and save it as `DataModule1.pas`. The Bold components that
are not visual live here, so every form can share them.

## Step 3: Add the Bold components

Drop these components on the data module:

| Component | Palette page | Purpose |
|-----------|--------------|---------|
| `TBoldModel` | Bold Handles | Holds the UML model |
| `TBoldSystemTypeInfoHandle` | Bold Handles | The run-time type information built from the model |
| [`TBoldSystemHandle`](../classes/TBoldSystemHandle.md) | Bold Handles | The object space at run time |
| `TBoldPersistenceHandleDB` | Bold Persistence | Loads and saves objects through a database adapter |

Connect them in the Object Inspector:

- `BoldSystemTypeInfoHandle1.BoldModel` → `BoldModel1`
- `BoldSystemHandle1.SystemTypeInfoHandle` → `BoldSystemTypeInfoHandle1`
- `BoldSystemHandle1.PersistenceHandle` → `BoldPersistenceHandleDB1`
- `BoldPersistenceHandleDB1.BoldModel` → `BoldModel1`

## Step 4: Connect the database

1. Drop a `TFDConnection` (FireDAC) on the data module. Set `DriverName` to `SQLite`, set `Params.Database` to a
   file name such as `Building.db`, and set `LoginPrompt` to `False`. FireDAC creates the file when it first
   connects.
2. Drop a `TBoldDatabaseAdapterFireDAC` (page Bold Persistence) and set:
    - `Connection` → `FDConnection1`
    - `DatabaseEngine` → `dbeGenericANSISQL92`, the SQL dialect Bold generates for SQLite
    - under `SQLDatabaseConfig`: `ColumnTypeForText`, `ColumnTypeForUnicodeText` and `ColumnTypeForAnsiText` →
      `TEXT`, and `ColumnTypeForInt64` → `INTEGER`, SQLite's own column types. This is the setup Bold's tests use.
3. Set `BoldPersistenceHandleDB1.DatabaseAdapter` → `BoldDatabaseAdapterFireDAC1`.

Another database needs another FireDAC driver and another `DatabaseEngine`;
[Persistence](../concepts/persistence.md#adapters) lists them. Every database except SQLite also needs its client
library; see [Troubleshooting](#database-client-libraries) below.

## Step 5: Create the model

1. Double-click `BoldModel1` to open the model editor.
2. Right-click in the tree and choose **New Class**. Bold adds a class called `NewClass`; change its **Name** to
   `Person`. Right-click the class and choose **New Attribute** for each attribute, and set its **Name** and
   **Type**:
    - `FirstName`: String
    - `LastName`: String
    - `Assets`: Currency
3. Add a second class, `Building`, with:
    - `Address`: String
    - `ZipCode`: Integer
4. Right-click the model's package and choose **New Association**. Name it `Ownership`. It has two ends,
   `AssociationEnd0` and `AssociationEnd1`; select each and set its **Name**, **Class** and **Multiplicity**. The
   class of an end is the class you reach through it:

    | End name | Class | Multiplicity |
    |----------|-------|--------------|
    | `OwnedBuildings` | Building | `0..*` |
    | `Owners` | Person | `0..*` |

    A person then has `OwnedBuildings` and a building has `Owners`.
5. Choose **File → Save and Generate All**. Bold validates the model, saves it to a `.bld` file (the first time it
   asks for the file name), and generates the business classes next to the data module: `BusinessClasses.pas` with
   `BusinessClasses_Interface.inc`, and `BusinessClasses.inc` for the bodies of any methods you add to the model.
   When the model has a persistence handle, it then asks whether to check the database for schema changes.
   **Tools → Generate Code** generates the classes without saving a `.bld` file.

The unit is called `BusinessClasses` unless you set the model's **Unit name** (select the model, the top node of
the tree).

## Step 6: Use the business classes

Add `BusinessClasses.pas` to the project, so that it is in the `uses` clause of the `.dpr` (its initialization
section registers the classes with Bold), and to the `uses` clause of the data module:

```pascal
uses
  BusinessClasses;
```

The unit holds a Delphi class per model class, `TPerson` and `TBuilding`, with a property per attribute and per
association end.

## Step 7: Design the main form

Add `DataModule1` to the main form's `uses` clause, then drop these components on the form.

**List handles** (page Bold Handles), one per list the form shows:

| Name | `RootHandle` | `Expression` |
|------|--------------|--------------|
| `blhAllPersons` | `DataModule1.BoldSystemHandle1` | `Person.allInstances` |
| `blhAllBuildings` | `DataModule1.BoldSystemHandle1` | `Building.allInstances` |

**Controls** (page Bold Controls):

- a `TBoldGrid` with `BoldHandle` = `blhAllPersons`
- a `TBoldGrid` with `BoldHandle` = `blhAllBuildings`
- a `TBoldNavigator` with `BoldHandle` = `blhAllPersons`, to add and delete persons

**Actions**: drop a `TActionList`, and add with **New Standard Action** (category Bold Actions):

- `TBoldActivateSystemAction`, which opens and closes the system
- `TBoldUpdateDBAction`, which saves the changes
- `TBoldCreateDatabaseAction`, which creates the tables

Set the `BoldSystemHandle` property of each action to `DataModule1.BoldSystemHandle1`, and give each one a button.

## Step 8: Run it

1. Press **F9**.
2. The first time, click the button of the create database action: Bold creates a table for each class.
3. Click the button of the activate action to open the system.
4. Add persons with the navigator and type in the grid. The grid shows each change at once.
5. Click the button of the update action to save.

That is a working Bold application. To create the tables in code instead, call
`BoldPersistenceHandleDB1.CreateDataBaseSchema` before the system is opened, for example when the database file does
not exist yet.

## Working with objects in code

The generated classes are ordinary Delphi classes that live in the object space.

### Create objects and links

```pascal
var
  Person: TPerson;
  Building: TBuilding;
begin
  Person := TPerson.Create(DataModule1.BoldSystemHandle1.System);
  Person.FirstName := 'John';
  Person.LastName := 'Doe';
  Person.Assets := 50000;

  Building := TBuilding.Create(DataModule1.BoldSystemHandle1.System);
  Building.Address := '123 Main Street';
  Building.ZipCode := 12345;

  Person.OwnedBuildings.Add(Building);   // Building.Owners now holds Person too
end;
```

### Query with OCL

```pascal
var
  RichPeople: TBoldObjectList;
  i: Integer;
begin
  RichPeople := DataModule1.BoldSystemHandle1.System.EvaluateExpressionAsNewElement(
    'Person.allInstances->select(assets > 100000)') as TBoldObjectList;
  try
    for i := 0 to RichPeople.Count - 1 do
      ShowMessage((RichPeople[i] as TPerson).FirstName);
  finally
    RichPeople.Free;   // AsNewElement: the caller owns the result
  end;
end;
```

### Save or discard changes

```pascal
DataModule1.BoldSystemHandle1.UpdateDatabase;   // write every change, in one database transaction
DataModule1.BoldSystemHandle1.System.Discard;   // or throw away the unsaved changes
```

## Troubleshooting

### Errors about missing tables when the system opens

The database has no schema yet. Run `TBoldCreateDatabaseAction` once, or call
`BoldPersistenceHandleDB1.CreateDataBaseSchema`.

### Database client libraries

FireDAC loads each database's client library at run time. Put the library next to the executable or on the `PATH`,
or name it in the connection's `VendorLib` parameter. The library must match the program: 64-bit for a Win64
program.

| Database | Library | Where to get it |
|----------|---------|-----------------|
| SQLite | none on Windows: FireDAC links SQLite in | — |
| PostgreSQL | `libpq.dll` and the DLLs next to it | the `bin` folder of a PostgreSQL installation |
| Firebird | `fbclient.dll` | the Firebird installation folder |
| MariaDB | `libmariadb.dll` | [MariaDB Connector/C](https://mariadb.com/downloads/connectors/) (MariaDB Server does not include it) |
| MySQL | `libmysql.dll` | the MySQL installation |
| Oracle | `oci.dll` | [Oracle Instant Client](https://www.oracle.com/database/technologies/instant-client/downloads.html); the database is named in Easy Connect form, `//host:port/service_name` |

## Next steps

- [OCL](../concepts/ocl.md): the query language
- [Persistence](../concepts/persistence.md): how objects map to tables
- [Examples](../examples/index.md): one example program per feature
