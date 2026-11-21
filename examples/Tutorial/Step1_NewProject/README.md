# Step 1: New Project Setup

In this step you learn what Bold is and create your first Bold project: the UML model, the object space, and the
persistence that stores objects in a database.

By the end, you have a working project that opens a Bold system and connects to SQLite.

## What You Will Learn

- What Bold for Delphi is
- How a model (`TBoldModel`) defines the structure of your application
- How the object space (`TBoldSystem`) holds your objects in memory
- How persistence works: how Bold talks to the database
- How Bold creates the database tables from your model

## What is Bold?

Bold for Delphi is a model-driven framework with an **object-relational mapper (ORM)**:

- **Object**: you work with ordinary Delphi classes (`TCustomer`, `TInvoice`) instead of SQL and tables. Bold
  generates these classes from a UML model.
- **Relational**: the data is stored in a relational database (SQLite, SQL Server, PostgreSQL and others).
- **Mapping**: Bold translates between the objects in memory and the rows in the database.

Without Bold, to load a customer you would write SQL:

```sql
SELECT * FROM Customer WHERE ...
```

and then create a Delphi object and copy the values from the result set by hand.

With Bold, you ask for the objects:

```pascal
var
  Customer: TCustomer;
begin
  Customer := dmInvoice.BoldSystemHandle1.System.ClassByExpressionName['Customer'][0] as TCustomer;
  ShowMessage(Customer.Name);  // Bold loaded the customer from the database
end;
```

Bold writes the SQL, reads the result set and creates the objects. You work with objects, not SQL.

## The Model

Bold uses a **UML model** to define the structure of your application. The model says:

- Which classes there are (Customer, Invoice, InvoiceItem)
- Which attributes they have (Customer.Name, Invoice.InvoiceDate)
- How they are related (a customer has many invoices)

This is the model you will create:

```
 ╔═══════════════════╗         ╔═════════════════════╗        ╔═════════════════════╗
 ║     Customer      ║         ║      Invoice        ║        ║    InvoiceItem      ║
 ╠═══════════════════╣         ╠═════════════════════╣        ╠═════════════════════╣
 ║ Name       String ║         ║ InvoiceNumber  Int  ║        ║ Description String  ║
 ║ Address    String ║         ║ InvoiceDate   Date  ║        ║ Quantity       Int  ║
 ║ City       String ║ 0..1  * ║ DueDate       Date  ║ 0..1 * ║ UnitPrice Currency  ║
 ║ Phone      String ╠═════════╣ IsPaid     Boolean  ╠════════╣ /LineTotal Currency ║
 ║ Email      String ║invoices ║ /TotalAmt  Currency ║ items  ║                     ║
 ╚═══════════════════╝         ╚═════════════════════╝        ╚═════════════════════╝
```

### How to Read the Diagram

**Associations** are the `═══` lines between classes. Each end has a multiplicity and a role name:

| Association | Meaning | Multiplicity |
|-------------|---------|--------------|
| Customer ═══ Invoice | A customer can have **many** invoices; an invoice has at most one customer | 0..1 to * |
| Invoice ═══ InvoiceItem | An invoice can have **many** line items; an item belongs to at most one invoice | 0..1 to * |

The role name (`invoices`, `items`) is how you navigate the association.

The **many** end gives you a **list**:

```pascal
MyCustomer.Invoices          // all invoices of this customer (a list)
MyCustomer.Invoices[0]       // the first invoice
MyCustomer.Invoices.Count    // how many invoices

MyInvoice.Items              // all line items of this invoice
MyInvoice.Items[2]           // the third item
```

The **one** end gives you a **single object**:

```pascal
MyInvoice.Customer           // the customer this invoice belongs to
MyInvoice.Customer.Name      // that customer's name

MyItem.Invoice               // the invoice this item belongs to
MyItem.Invoice.InvoiceNumber // that invoice's number
```

**Derived attributes** have a `/` prefix. Bold calculates them from an OCL expression:

| Attribute | OCL expression | Meaning |
|-----------|----------------|---------|
| `/TotalAmount` | `items.lineTotal->sum` | Sum of the line totals of all items |
| `/LineTotal` | `quantity * unitPrice` | This item's quantity times its unit price |

You never set these values; they follow the data they are calculated from. You add them in Step 3.

You create the model in the **Bold model editor**, inside the Delphi IDE.

## Build It Yourself (Recommended)

Create everything from scratch. The `Shared/` folder holds the finished files as an answer key; open them whenever
you get stuck.

### 1. Create the project and the data module

1. Start from an empty VCL application. The step folder has one, `Step1User.dproj`; `Step_1.groupproj` opens it
   together with the finished `Step1.dproj`. You can also create your own with **File > New > Windows VCL
   Application**.
2. Give the project Bold's search path: copy it from `Step1.dproj` (**Project > Options > Building > Delphi
   Compiler > Search path**). Without it the project does not find Bold's units.
3. Add a data module with **File > New > Other > Delphi Files > Data Module** and save it as
   `MyInvoiceDataModule.pas`.
4. Drop a `TBoldModel` from the **Bold Handles** page of the Tool Palette on the data module.
5. Double-click `BoldModel1` to open the **Bold model editor**.

If the Bold pages are missing from the Tool Palette, the Bold design package is not installed. See Troubleshooting
in the [tutorial README](../README.md#troubleshooting).

### 2. Name the generated unit

Select the model, the top node of the tree, and set its **Unit name** to `InvoiceClasses`. The code generator names
the unit after it; the default is `BusinessClasses`.

### 3. Create the Customer class

1. Right-click in the tree and choose **New Class**. Bold adds a class called `NewClass`.
2. Change its **Name** to `Customer`.
3. Right-click the class and choose **New Attribute**. Bold adds `NewAttribute`, of type String. Set its **Name**
   and **Type**. Add these attributes:
   - Name (String)
   - Address (String)
   - City (String)
   - Phone (String)
   - Email (String)

### 4. Create the Invoice class

Add the class `Invoice` with these attributes:

- InvoiceNumber (Integer)
- InvoiceDate (Date)
- DueDate (Date)
- IsPaid (Boolean)

Do not add TotalAmount yet. It becomes a derived attribute in Step 3.

### 5. Create the InvoiceItem class

Add the class `InvoiceItem` with these attributes:

- Description (String)
- Quantity (Integer)
- UnitPrice (Currency)

Do not add LineTotal yet. It also becomes a derived attribute in Step 3.

### 6. Create the associations

Right-click the model's package and choose **New Association**. Bold adds an association with two ends,
`AssociationEnd0` and `AssociationEnd1`. Select each end and set its **Name**, **Class** and **Multiplicity**. The
class of an end is the class you reach through it.

**Association 1**: name it `CustomerInvoices`. A customer has many invoices.

| End name | Class | Multiplicity |
|----------|-------|--------------|
| `Invoices` | Invoice | `*` |
| `Customer` | Customer | `0..1` |

**Association 2**: name it `InvoiceItems`. An invoice has many items.

| End name | Class | Multiplicity |
|----------|-------|--------------|
| `Items` | InvoiceItem | `*` |
| `Invoice` | Invoice | `0..1` |

In Delphi the names stay as you typed them (`MyCustomer.Invoices`). In OCL a member name starts with a lowercase
letter (`invoices`).

### 7. Generate the code

Choose **Tools > Generate Code** in the model editor. Bold checks the model and writes the business classes into
the folder of the data module:

- `InvoiceClasses.pas`: your classes (`TCustomer`, `TInvoice`, `TInvoiceItem`)
- `InvoiceClasses_Interface.inc`: their declarations and properties

**File > Save and Generate All** does the same and also saves the model to a `.bld` file; the first time it asks
for the file name.

Then:

1. Add `InvoiceClasses.pas` to the project. It must be in the `uses` clause of the `.dpr`: its initialization
   section registers the classes with Bold.
2. Save the data module. The model is stored in its `.dfm`.

**Compare your result** with `Shared/InvoiceClasses.pas` and `Shared/InvoiceClasses_Interface.inc`.

### 8. Complete the data module

Drop these components on the data module and set their properties as in the table under
[The Components](#the-components) below:

- a `TBoldSystemTypeInfoHandle` and a `TBoldSystemHandle` (page **Bold Handles**)
- a `TActionList` with a `TBoldActivateSystemAction` (right-click the action list, **New Standard Action**,
  category **Bold Actions**)

Then copy the code of `Shared/InvoiceDataModule.pas` into your unit: the units of its `uses` clause, the private
fields, `GetIniFileName`, `DatabaseName`, `SetupPersistence`, `EnsureDatabaseExists`, `OpenSystem` and the event
handlers. Without an `.ini` file next to the executable, `DatabaseName` names the database after the executable,
for example `Step1User.db`. The next section explains what the code does.

## Use the Finished Files (Quick Start)

To skip the model creation and see how Bold works first, use `Step1.dproj`. It is ready to compile and run, with
the files in `Shared/`:

- `InvoiceDataModule.pas` and `.dfm`: the data module with the model and the persistence
- `InvoiceClasses.pas` and `InvoiceClasses_Interface.inc`: the generated business classes
- `InvoiceModel.bld`: the model as a `.bld` file

## The Data Module

The data module (`InvoiceDataModule.pas`) holds the model, the system handle, the persistence and the database
connection. Every form of the application uses it.

### The Components

| Component | Class | Set in | Points to |
|-----------|-------|--------|-----------|
| `BoldModel1` | `TBoldModel` | `.dfm` | the model, stored in the `.dfm` |
| `BoldSystemTypeInfoHandle1` | `TBoldSystemTypeInfoHandle` | `.dfm` | `BoldModel` = `BoldModel1` |
| `BoldSystemHandle1` | `TBoldSystemHandle` | `.dfm` | `SystemTypeInfoHandle` = `BoldSystemTypeInfoHandle1`, `IsDefault` = True, `PersistenceHandle` = `FPersistenceHandleDB` (set in code) |
| `BoldActivateSystemAction1` | `TBoldActivateSystemAction` | `.dfm` | `BoldSystemHandle` = `BoldSystemHandle1` |
| `FFDConnection` | `TFDConnection` | code | the SQLite file |
| `FFireDACAdapter` | `TBoldDatabaseAdapterFireDAC` | code | `Connection` = `FFDConnection` |
| `FPersistenceHandleDB` | `TBoldPersistenceHandleDB` | code | `BoldModel` = `BoldModel1`, `DatabaseAdapter` = `FFireDACAdapter` |

The last three are created in code, in `SetupPersistence`, so that you can read exactly what each piece does. You
could drop them on the data module instead; the classes are on the **Bold Persistence** page and the FireDAC pages
of the Tool Palette.

#### TBoldModel

Holds your UML model. The model is stored in the data module's `.dfm`; the model editor edits it there. Bold reads
from it which classes there are, which attributes they have and which associations connect them.

#### TBoldSystemTypeInfoHandle

Builds the run-time type information from the model: what the classes look like when the application runs. The
system handle and the other handles get the type information from it.

#### TBoldSystemHandle

Creates and manages the **object space**, the `TBoldSystem`. The object space is the heart of Bold:

- It holds your objects in memory.
- A customer you load from the database lives in the object space.
- A customer you create is added to the object space.
- When you change a customer's name, the object space marks the customer as **dirty**.
- When you call `UpdateDatabase`, it writes all dirty objects to the database.

**Key concept**: the object space is not the database. It is a separate set of objects in memory, and Bold keeps
the two in step.

#### TFDConnection (FireDAC)

```
FFDConnection: TFDConnection
  DriverID: SQLite
  Database: Step1.db (from Step1.ini)
  CreateDatabase=True
```

Opens the SQLite database. FireDAC is Delphi's database access library; `CreateDatabase=True` makes SQLite create
the file when it does not exist.

#### TBoldDatabaseAdapterFireDAC

```
FFireDACAdapter: TBoldDatabaseAdapterFireDAC
  Connection: FFDConnection
  DatabaseEngine: dbeInterbaseSQLDialect3
```

Lets Bold use a FireDAC connection. `DatabaseEngine` tells Bold which SQL dialect to generate.

#### TBoldPersistenceHandleDB

```
FPersistenceHandleDB: TBoldPersistenceHandleDB
  BoldModel: BoldModel1
  DatabaseAdapter: FFireDACAdapter
```

Connects the object space to the database: `BoldSystemHandle1.PersistenceHandle` points to it. When you call
`UpdateDatabase`:

1. Bold collects the dirty objects of the object space.
2. The persistence handle generates the SQL INSERT, UPDATE and DELETE statements.
3. It runs them through the adapter and FireDAC.
4. The database is updated.

#### TBoldActivateSystemAction

```
BoldActivateSystemAction1: TBoldActivateSystemAction
  BoldSystemHandle: BoldSystemHandle1
  OnExecute: BoldActivateSystemAction1Execute
```

Opens and closes the object space; its caption changes between "Open system" and "Close system". In this tutorial
its `OnExecute` handler in the data module does the work:

- **Opening**: `EnsureDatabaseExists` creates the database tables if there are none, then the handler sets
  `BoldSystemHandle1.Active := True`.
- **Closing**: if there are dirty objects, it asks whether to save them (`UpdateDatabase`), discard them
  (`Discard`) or cancel, then sets `Active := False`.

The action fires `OnSystemOpened` when the system opens and `OnSystemClosed` when it closes.

### Putting It Together

When the data module is created, it builds the persistence chain:

```pascal
procedure TInvoiceDataModule.DataModuleCreate(Sender: TObject);
begin
  SetupPersistence;
end;
```

`SetupPersistence` creates the connection, the adapter and the persistence handle and connects them to
`BoldSystemHandle1`. When the form then opens the system (`OpenSystem`, which executes
`BoldActivateSystemAction1`), `EnsureDatabaseExists` checks whether Bold's table `BOLD_TYPE` exists. If it does
not, `CreateDataBaseSchema` creates all tables from the model.

### Persistence: How Objects Get to the Database

1. **Create**: you create an object: `Customer := TCustomer.Create(dmInvoice.BoldSystemHandle1.System);`
2. **Modify**: you change attributes: `Customer.Name := 'Acme Corp';`
3. **Track**: the object space marks the customer as dirty.
4. **Save**: you call `dmInvoice.BoldSystemHandle1.System.UpdateDatabase;`
5. **SQL**: Bold generates the INSERT statements for the customer's tables.
6. **Commit**: the statements run in one transaction.
7. **Clean**: the object space clears the dirty marks.

**Important**: the database does not see changes in the object space until you call `UpdateDatabase`. If you
close the application without saving, the changes are lost. This is why every form of the tutorial asks before it
closes.

## The Form

Step 1's form (`Step1Form.pas`) only shows that the system opens:

```pascal
procedure TfrmStep1.FormCreate(Sender: TObject);
begin
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmInvoice.OpenSystem;  // open the object space
end;

procedure TfrmStep1.HandleSystemOpened(Sender: TObject);
begin
  UpdateStatusLabels;  // shows "Bold System: Active" and the database file
end;
```

When you run the application:

1. The form calls `OpenSystem`.
2. The data module creates the database tables on the first run and activates the system.
3. `OnSystemOpened` fires and the labels update.
4. The form shows the `.ini` file, the database file and "Bold System: Active".

The **Open system / Close system** button is connected to `BoldActivateSystemAction1`. When you close the form
with unsaved changes, `FormCloseQuery` asks whether to save them.

That is all for Step 1: it lays the foundation. The UI work starts in Step 2.

## Key Concepts

### The Object Space Holds Your Objects

Think of the object space as a cache of objects:

- **Before**: you query the database with SQL every time you need data.
- **Now**: Bold loads objects into the object space, and you work with them in memory.
- **Benefit**: fast access, change tracking, and typed Delphi classes.

### Persistence Without SQL

You change objects, and Bold handles the database:

```pascal
Customer.Name := 'New Name';                        // change the object in memory
dmInvoice.BoldSystemHandle1.System.UpdateDatabase;  // Bold generates the SQL and updates the database
```

### The Model Is the Source of Truth

The UML model you create in the model editor defines the structure of your application. The generated Delphi
classes follow the model: add an attribute to the model, generate the code again, and the Delphi class has the new
property.

## Try It Yourself

### Exercise 1: Add an Attribute

Do this in the project you built yourself; the model in `Shared/` is the one Steps 2 to 4 use.

1. Double-click `BoldModel1` to open the model editor.
2. Add the attribute `Website` (String) to the Customer class.
3. Choose **Tools > Generate Code** and rebuild the project.
4. Delete the step's `.db` file: the data module creates the tables only when the database has none, so the new
   column needs a fresh database.

`TCustomer` now has a `Website` property. Try it in the form (add `InvoiceClasses` to its `uses` clause, and use
the name of your data module where the answer key has `dmInvoice`):

```pascal
var
  Customer: TCustomer;
begin
  Customer := TCustomer.Create(dmInvoice.BoldSystemHandle1.System);
  Customer.Website := 'https://example.com';
  ShowMessage(Customer.Website);
end;
```

### Exercise 2: Change the Database File

The application stores its data in `Step1.db`, next to the executable.

1. Open `Step1.ini`, next to the executable.
2. Change the file name in the `[SQLite]` section: `Database=MyData.db`
3. Run the application again.

Bold creates the new database file; `Step1.db` stays as it was. The location of the database is configuration, not
code.

### Exercise 3: Inspect the Database

Open the SQLite file in any SQLite viewer, for example [DB Browser for SQLite](https://sqlitebrowser.org/):

1. Stop the application.
2. Open `Step1.db` in the viewer.
3. Look at the tables: one per class (Customer, Invoice, InvoiceItem), plus Bold's own tables, whose names start
   with `BOLD_`.
4. The columns of a class table match the attributes of the class in the model.

Bold created this schema from the model.

## Next Step

The foundation is in place. Go to [`Step2_CustomerCRUD/README.md`](../Step2_CustomerCRUD/README.md) to create,
read, update and delete customers through a user interface.

---

**Key Takeaways**:

- Bold maps Delphi objects to database tables
- The model (`TBoldModel`) defines your classes; Bold generates the Delphi code from it
- The object space (`TBoldSystem`) holds your objects in memory
- The data module wires the model, the object space and the database together
- `UpdateDatabase` writes all changes to the database in one transaction
