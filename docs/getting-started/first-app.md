# First Application

This guide walks you through creating your first Bold application - a simple app with **Person** and **Building** objects where persons can own buildings.

!!! tip "Quick Alternative"
    Want to skip ahead? Open the ready-made example at:
    `examples\Simple\ObjectSpace\MasterDetail\MasterDetail.dpr`

## Step 1: Create a New VCL Application

1. **File → New → VCL Forms Application**
2. Save the project (e.g., `Building.dproj`)

## Step 2: Add a Data Module

1. **File → New → Other → Delphi Files → Data Module**
2. Save as `DataModule1.pas`

## Step 3: Add Core Bold Components

Drop these components on your DataModule from the Bold palette:

| Component | Purpose |
|-----------|---------|
| `TBoldModel` | Holds your UML model |
| `TBoldSystemHandle` | Main runtime system |
| `TBoldSystemTypeInfoHandle` | Type information for design-time |
| `TBoldPersistenceHandleDB` | Database persistence |

Connect them:

- `BoldSystemHandle1.BoldModel` → `BoldModel1`
- `BoldSystemHandle1.PersistenceHandle` → `BoldPersistenceHandleDB1`
- `BoldSystemTypeInfoHandle1.BoldModel` → `BoldModel1`

## Step 4: Add Database Connection

1. Drop a `TFDConnection` (FireDAC) on the DataModule
2. Drop a `TBoldDatabaseAdapterFireDAC` component
3. Connect them:
   - `BoldDatabaseAdapterFireDAC1.Connection` → `FDConnection1`
   - `BoldPersistenceHandleDB1.DatabaseAdapter` → `BoldDatabaseAdapterFireDAC1`

### Database Configuration

Create an INI file with your database settings. Here are examples for each supported database:

=== "SQLite (Easiest)"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=SQLite

    [SQLite]
    Database=BoldDemo.db
    ```

    No server installation needed - the database file is created automatically.

=== "SQL Server"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=MSSQL

    [MSSQL]
    Server=localhost
    Database=BoldDemo
    OSAuthent=True
    User=sa
    Password=
    ```

=== "PostgreSQL"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=PostgreSQL

    [PostgreSQL]
    Server=localhost
    Database=bolddemo
    VendorLib=C:\Program Files\PostgreSQL\18\bin\libpq.dll
    User=postgres
    Password=<your_password>
    ```

=== "Firebird"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=Firebird

    [Firebird]
    Server=localhost
    Database=C:\Data\BoldDemo.fdb
    VendorLib=C:\Program Files\Firebird\Firebird_5_0\fbclient.dll
    User=SYSDBA
    Password=<your_password>
    ```

=== "MariaDB/MySQL"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=MariaDB

    [MariaDB]
    Server=localhost
    Port=3306
    Database=bolddemo
    User=root
    Password=<your_password>
    VendorLib=C:\Program Files\MariaDB\MariaDB Connector C 64-bit\lib\libmariadb.dll
    ```

=== "Oracle"

    ```ini
    [Database]
    Persistence=FireDAC
    Type=Oracle

    [Oracle]
    Database=//localhost:1521/XEPDB1
    User=bolduser
    Password=<your_password>
    VendorLib=C:\oracle\instantclient_23_7\oci.dll
    ```

## Step 5: Create Your Model

1. Double-click `BoldModel1` to open the Model Editor

2. **Create the Person class:**
   - Right-click → Add Class → Name: `Person`
   - Add attributes:
     - `FirstName`: String
     - `LastName`: String
     - `Assets`: Currency

3. **Create the Building class:**
   - Add Class → Name: `Building`
   - Add attributes:
     - `Address`: String
     - `ZipCode`: Integer

4. **Create an association (Ownership):**
   - Select both classes
   - Right-click → Add Association
   - Name: `Ownership`
   - Person side: `OwnedBuildings` (0..*)
   - Building side: `Owners` (0..*)

5. **File → Save & Generate all files**

This creates `BusinessClasses.pas` with strongly-typed classes.

## Step 6: Add Business Classes to Project

1. Add the generated `BusinessClasses.pas` to your project
2. Add to DataModule uses clause:

```pascal
uses
  BusinessClasses;
```

## Step 7: Design the Main Form

On your main form, add:

### List Handles (from Bold Handles palette)

```pascal
// TBoldListHandle named blhAllPersons
blhAllPersons.RootHandle := DataModule1.BoldSystemHandle1;
blhAllPersons.Expression := 'Person.allInstances';

// TBoldListHandle named blhAllBuildings
blhAllBuildings.RootHandle := DataModule1.BoldSystemHandle1;
blhAllBuildings.Expression := 'Building.allInstances';
```

### GUI Components (from Bold Controls palette)

- `TBoldGrid` for persons → `BoldHandle` = `blhAllPersons`
- `TBoldGrid` for buildings → `BoldHandle` = `blhAllBuildings`
- `TBoldNavigator` → `BoldHandle` = `blhAllPersons`

### Actions (from Bold Actions palette)

- `TBoldActivateSystemAction` → Opens the database
- `TBoldUpdateDBAction` → Saves changes to database
- Add buttons linked to these actions

## Step 8: Initialize the System

In your main form's OnCreate:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activate opens/creates the database and loads the model
  BoldActivateSystemAction1.Execute;
end;
```

## Step 9: Run It!

1. Press **F9** to run
2. Click **Open System** to activate
3. Use the navigator to add Person records
4. The grid automatically displays them
5. Click **Update DB** to save

**Congratulations!** You have a working Bold application.

## Working with Objects in Code

### Creating Objects

```pascal
var
  Person: TPerson;
  Building: TBuilding;
begin
  // Create objects - Bold tracks them automatically
  Person := TPerson.Create(BoldSystemHandle1.System);
  Person.FirstName := 'John';
  Person.LastName := 'Doe';
  Person.Assets := 50000;

  Building := TBuilding.Create(BoldSystemHandle1.System);
  Building.Address := '123 Main Street';
  Building.ZipCode := 12345;

  // Create relationship
  Person.OwnedBuildings.Add(Building);
end;
```

### Querying with OCL

```pascal
var
  RichPeople: TBoldObjectList;
begin
  // Find all persons with assets > 100000
  RichPeople := BoldSystemHandle1.System.EvaluateExpressionAsNewElement(
    'Person.allInstances->select(assets > 100000)'
  ) as TBoldObjectList;

  // Iterate results
  for var i := 0 to RichPeople.Count - 1 do
    ShowMessage(TPerson(RichPeople[i]).FirstName);
end;
```

### Saving to Database

```pascal
// Save all changes
BoldSystemHandle1.UpdateDatabase;

// Or discard changes
BoldSystemHandle1.System.Discard;
```

## Troubleshooting

### Database errors on first run

Use `TBoldCreateDatabaseAction` to create the schema first, or call:

```pascal
BoldPersistenceHandleDB1.CreateDataBaseSchema;
```

### "libpq.dll not found" (PostgreSQL)

Copy `libpq.dll` and dependencies from PostgreSQL's `bin` folder to your executable folder.

### "fbclient.dll not found" (Firebird)

Copy `fbclient.dll` from your Firebird installation folder.

### "libmysql.dll not found" (MariaDB/MySQL)

Download MariaDB Connector/C from [mariadb.com](https://mariadb.com/downloads/connectors/) and set VendorLib path.

### "oci.dll not found" (Oracle)

Download Oracle Instant Client and set VendorLib to point to `oci.dll`.

## Next Steps

- [OCL Queries](../concepts/ocl.md) - Learn the query language
- [Persistence](../concepts/persistence.md) - Understand database mapping
- [Examples](../examples/index.md) - Explore more examples
