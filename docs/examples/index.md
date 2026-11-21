# Examples

The `examples/` folder holds more than 40 example programs. Each folder has a `README.md` that says what the example
shows, how it works and how to run it; the links below open it on GitHub.

## Where to start

| Example | Toolchain | What it shows |
|---------|-----------|---------------|
| [MasterDetail](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/MasterDetail) | Delphi, VCL | A detail list that follows the master, with no code. The demo to start with |
| [MasterDetail FMX](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/MasterDetail/FMX) | Delphi, FMX, Win64 and Linux64 | The same demo through `TBoldDataSet` and LiveBindings |
| [Invoicing](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing) | Delphi and Lazarus | One model with an FMX client, an LCL client and console runners |
| [Tutorial](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial) | Delphi, VCL | A five-step course that builds an invoicing application |
| [OCL Workbench](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/Tools/OclWorkbench) | Delphi, VCL | Type an OCL expression, run it against a live system, see what comes back |

## Which examples run today

The examples come from two generations:

- **Maintained examples** have a current project file (`.dproj` for Delphi, `.lpi` for Lazarus) and use FireDAC,
  Zeos or XML file persistence. They build and run as they are. These are the tutorial, MasterDetail and its FMX
  twin, Invoicing, OCL Workbench, Save and Generate, LogBridge, ASP and Conway.
- **Original Boldsoft demos** have only a `.dpr`, and most of them use the InterBase adapter (`TBoldDatabaseAdapterIB`,
  IBX), which is under `Source/Deprecated` and not in the maintained packages. Delphi creates a `.dproj` when you open
  the `.dpr`, but the program needs its data module ported to FireDAC before it runs. Their READMEs are still worth
  reading: each one explains a feature with the code that implements it.

Every demo with a VCL or COM user interface runs on Windows only.

## Database configuration

MasterDetail, OCL Workbench and Save and Generate share one data module, `examples/Shared/DemoDataModule.pas`. It
reads the database settings from the `.ini` file that has the program's name and sits next to the program, for
example `MasterDetail.ini`. `MasterDetail.ini` selects SQLite, which needs no server; `OclWorkbench.ini` and
`TestSaveAndGenerate.ini` select SQL Server, so change their `Type` to `SQLite` if you have no SQL Server.

The `[Database]` section chooses the persistence and the database:

```ini
[Database]
; FireDAC, UniDAC (only in a build that defines UNIDAC) or XML
Persistence=FireDAC
; MSSQL, PostgreSQL, Firebird, SQLite, MariaDB or Oracle; ignored for XML
Type=SQLite
```

Each database type has a section of its own:

=== "SQLite"

    ```ini
    [SQLite]
    Database=BoldDemo.db
    ```

    The file is created on the first run.

=== "SQL Server"

    ```ini
    [MSSQL]
    Server=localhost
    Database=BoldDemo
    ; True: Windows authentication; False: User and Password
    OSAuthent=True
    User=sa
    Password=
    ```

=== "PostgreSQL"

    ```ini
    [PostgreSQL]
    Server=localhost
    Port=5432
    Database=bolddemo
    User=postgres
    Password=<your password>
    VendorLib=C:\Program Files\PostgreSQL\18\bin\libpq.dll
    ```

=== "Firebird"

    ```ini
    [Firebird]
    Server=localhost
    Database=C:\Data\BoldDemo.fdb
    User=SYSDBA
    Password=<your password>
    VendorLib=C:\Program Files\Firebird\Firebird_5_0\fbclient.dll
    ```

=== "MariaDB / MySQL"

    ```ini
    [MariaDB]
    Server=localhost
    Port=3306
    Database=bolddemo
    User=root
    Password=<your password>
    ; libmariadb.dll from MariaDB Connector/C, or libmysql.dll for MySQL
    VendorLib=C:\Program Files\MariaDB\MariaDB Connector C 64-bit\lib\libmariadb.dll
    ```

=== "Oracle"

    ```ini
    [Oracle]
    ; Easy Connect: host:port/service
    Database=localhost:1521/FREEPDB1
    ; the schema
    User=bolddemo
    Password=<your password>
    ; used to create the schema user when it does not exist
    AdminUser=SYSTEM
    AdminPassword=<admin password>
    VendorLib=C:\oracle\instantclient_23_7\oci.dll
    ```

=== "XML file"

    ```ini
    [Database]
    Persistence=XML

    [XML]
    ; defaults to <program name>.xml
    FileName=BoldDemo.xml
    ; keep the data in memory between operations
    CacheData=True
    ```

Leave out a key to use its default. The client libraries named in `VendorLib` are described under
[Database client libraries](../getting-started/first-app.md#database-client-libraries).

## Tutorial

`examples/Tutorial`: a five-step course for newcomers, in Delphi with the VCL and SQLite through FireDAC. Each step
has a finished project; steps 1 and 2 also have a starting project to build the step yourself.

| Step | Topic |
|------|-------|
| [1. New project](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial/Step1_NewProject) | The model, the system, persistence to SQLite |
| [2. Customer CRUD](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial/Step2_CustomerCRUD) | List handles, grids and navigators |
| [3. Invoices](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial/Step3_Invoices) | Associations and handle chaining for master-detail |
| [4. OCL expressions](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial/Step4_OclExpressions) | Filtering, sorting and aggregating with OCL; derived attributes |
| [5. Inheritance and VAT](https://github.com/bero/BoldForDelphi/tree/develop/examples/Tutorial/Step5_InheritanceVAT) | Inheritance (Company and PrivatePerson) and a lookup class for VAT rates |

## Object space

In `examples/Simple/ObjectSpace`. All are Delphi VCL programs.

| Example | What it shows | Runs today |
|---------|---------------|------------|
| [MasterDetail](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/MasterDetail) | A list handle rooted on another list handle | Yes (FireDAC, UniDAC or XML) |
| [AssociationClass](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/AssociationClass) | An association that carries data of its own | `.dpr`, IBX |
| [Constraints](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/Constraints) | OCL constraints on classes and attributes | `.dpr`, IBX |
| [CustomAttributes](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/CustomAttributes) | Attribute types of your own | `.dpr`, IBX |
| [DataSetAndReporting](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/DataSetAndReporting) | `TBoldDataSet`: a Bold list as a `TDataSet` for reports | `.dpr`, IBX, QuickReport |
| [DerivedAttributes](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/DerivedAttributes) | Attributes computed from OCL and kept current | `.dpr`, IBX |
| [DerivedHandle](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/DerivedHandle) | `TBoldDerivedHandle`: a value computed in Delphi code | `.dpr`, IBX |
| [DerivedRelations](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/DerivedRelations) | Associations derived by OCL | `.dpr`, IBX |
| [OclVariables](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/OclVariables) | `TBoldOclVariables`: values from outside the object in an expression | `.dpr`, IBX |
| [OptimisticLocking](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/OptimisticLocking) | Detecting that someone else saved the object first | `.dpr`, IBX |
| [ReverseDerivedAttributes](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/ReverseDerivedAttributes) | Derived attributes that can be written | `.dpr`, IBX |
| [SQLHandle](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/SQLHandle) | `TBoldSQLHandle`: fetching objects with a SQL `WHERE` clause | `.dpr`, IBX |
| [Temporal](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/Temporal) | Object versioning: every save keeps the old version | `.dpr`, IBX |
| [Transaction](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/ObjectSpace/Transaction) | Object-space transactions: all or nothing in memory | `.dpr`, IBX |

## User interface controls

In `examples/Simple/GUIControls`. All are Delphi VCL programs with a `.dpr` and IBX.

| Example | What it shows |
|---------|---------------|
| [BoldImage](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/BoldImage) | `TBoldImage` and typed blobs with a MIME content type |
| [ComboBox](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/ComboBox) | `TBoldComboBox`: the edit area and the list follow different handles |
| [MultiLang](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/MultiLang) | Attributes with one value per language |
| [Renderers](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/Renderers) | Renderers: custom display, editing and colouring of a value |
| [SimpleTreeView](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/SimpleTreeView) | `TBoldTreeView` built from node descriptions |
| [TreeView](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/GUIControls/TreeView) | `TBoldTreeView` with caption nodes and more |

## Tools and infrastructure

| Example | What it shows | Runs today |
|---------|---------------|------------|
| [OclWorkbench](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/Tools/OclWorkbench) | An OCL console against a live system | Yes, Delphi VCL |
| [SaveAndGenerateTest](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/Tools/SaveAndGenerateTest) | Saving a model and generating code from application code | Yes, Delphi VCL |
| [LogBridge](https://github.com/bero/BoldForDelphi/tree/develop/examples/Simple/LogBridge) | Sending Bold's log to your application's logging | Yes, Delphi console (Win32 project) |
| [DefaultProject](https://github.com/bero/BoldForDelphi/tree/develop/examples/Repository/DefaultProject) | The minimal skeleton of a Bold application | `.dpr` |

## Larger applications

In `examples/Compound`.

| Example | What it shows | Runs today |
|---------|---------------|------------|
| [Invoicing](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/LCL) | One model, an FMX client, an LCL client, console runners and tests, on SQLite or XML | Yes: [FMX](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/FMX) (Delphi), [LCL](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/LCL) and [Zeos](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Invoicing/Zeos) (Lazarus) |
| [ASP](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ASP) | An object space served over HTTP by an ISAPI extension | Yes, Delphi, Windows |
| [Conway](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Conway) | Game of Life computed entirely by OCL derivations | Yes, Delphi VCL, no database |
| [Building](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Building) | Nearly every Bold-aware VCL feature in one form | `.dpr`, FireDAC |
| [Bios](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Bios) | The Building demo with the object space in a COM server | `.dpr`, IBX, COM |
| [bios2](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/bios2) | Remote navigation compared with a COM service object | `.dpr`, IBX, COM |
| [ExternalPersistence](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ExternalPersistence) | Objects read from a database Bold does not own | `.dpr`, BDE and IBX |
| [ExternalPersistence2](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ExternalPersistence2) | The same, with the mapping declared in the model | `.dpr`, IBX |
| [HTTPPMapper](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/HTTPPMapper) | Three tiers: Bold's persistence protocol over HTTP | `.dpr`, IBX |
| [Locking](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/Locking) | Pessimistic locking with a lock manager, and change propagation | `.dpr`, IBX, COM |
| [ModelEvolution](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ModelEvolution) | Evolving the database and upgrading objects to a new model | `.dpr`, IBX |
| [ProdStruct](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/ProdStruct) | A bill of materials, derivation in code, a tree view | `.dpr`, IBX |
| [XML](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/XML) | A model served to a browser as XML and edited back | `.dproj`, IBX, COM, ISAPI |
| [XML2](https://github.com/bero/BoldForDelphi/tree/develop/examples/Compound/XML2) | A generic XML exporter, OCL sent from the browser | `.dpr`, IBX, COM, ISAPI |

## PDF guides

The `Doc/` folder has the original Boldsoft guides. They describe Bold 4.0 in Delphi 7, so menus and versions
differ, but the concepts are the same.

| Guide | Topic |
|-------|-------|
| [Starting Bfd, Part 1](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%201%20-%20Introducing%20the%20Basics.pdf) | Introducing the basics |
| [Starting Bfd, Part 2](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%202%20-%20Extending%20Models.pdf) | Extending models |
| [Starting Bfd, Part 3](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Starting%20Bfd%20-%20Part%203%20-%20OCL.pdf) | OCL |
| [Delphi and Model Driven Architecture](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Delphi%20and%20model%20Driven%20Architecture/Delphi%20and%20Model%20Driven%20Architecture.pdf) | MDA with Bold, at length |
| [Creating custom Bold-aware components](https://github.com/bero/BoldForDelphi/blob/develop/Doc/Creating%20custom%20Bold-aware%20components.pdf) | Writing your own Bold-aware controls |
| [UML 1.1 OCL specification](https://github.com/bero/BoldForDelphi/blob/develop/Doc/ad970808_UML11_OCL.pdf) | The OCL language reference |

More is in the [Doc folder on GitHub](https://github.com/bero/BoldForDelphi/tree/develop/Doc).
