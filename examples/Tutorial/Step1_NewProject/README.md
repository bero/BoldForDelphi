# Step 1: New Project Setup

In this step, you will learn what Bold is and how to create your first Bold project. We start with the fundamentals: the BoldSystem, the UML Model, and how Bold persists objects to a database.

By the end, you will have a working project that opens a Bold system and connects to SQLite.

## What You Will Learn

- What is Bold for Delphi?
- What is a BoldModel and how does it define your application's structure?
- What is a BoldSystem and how does it hold all your objects in memory?
- How does persistence work? How does Bold talk to the database?
- How does Bold auto-create database tables from your model?

## What is Bold?

Bold for Delphi is an **Object-Relational Mapping (ORM) framework**. Here's what that means:

- **Object-oriented**: You work with normal Delphi classes (TCustomer, TInvoice) instead of SQL and tables
- **Relational**: Data is stored in a traditional relational database (SQLite, SQL Server, PostgreSQL, etc.)
- **Mapping**: Bold automatically translates between objects in memory and rows in the database

Without Bold, if you wanted to load a customer, you would write SQL:

```sql
SELECT * FROM Customers WHERE Id = 1
```

Then manually create a Delphi object and copy values from the result set.

With Bold, you write one line:

```pascal
var Customer: TCustomer;
begin
  Customer := TCustomer(BoldSystem.Get(1));
  ShowMessage(Customer.Name);  // Bold loaded the name from the database
end;
```

Bold handles the SQL, the result set parsing, and the object creation. You work with objects, not SQL.

![Step 1 running - Bold System Open](../images/step1-running.png)
*The Step 1 application showing "Bold System: OPEN" — your first Bold project is working*

## Creating the Bold Model

Bold uses a **UML Model** to define the structure of your application. The model says:

- What classes do you have? (Customer, Invoice, InvoiceItem)
- What attributes do they have? (Customer.Name, Invoice.InvoiceDate)
- What are the relationships? (Customer has many Invoices)

Here is the complete model you will create:

```
 ╔═══════════════════╗         ╔═════════════════════╗        ╔═════════════════════╗
 ║     Customer      ║         ║      Invoice        ║        ║    InvoiceItem      ║
 ╠═══════════════════╣         ╠═════════════════════╣        ╠═════════════════════╣
 ║ Name       String ║         ║ InvoiceNumber  Int  ║        ║ Description String  ║
 ║ Address    String ║         ║ InvoiceDate   Date  ║        ║ Quantity       Int  ║
 ║ City       String ║ 1     * ║ DueDate       Date  ║ 1    * ║ UnitPrice Currency  ║
 ║ Phone      String ╠═════════╣ IsPaid     Boolean  ╠════════╣ /LineTotal Currency ║
 ║ Email      String ║invoices ║ /TotalAmt  Currency ║ items  ║                     ║
 ╚═══════════════════╝         ╚═════════════════════╝        ╚═════════════════════╝
```

### How to Read the Diagram

**Associations** are the `═══` lines between classes. Each has a multiplicity (`1` and `*`) and a role name:

| Association | Meaning | Multiplicity |
|------------|---------|-------------|
| Customer ═══ Invoice | One customer can have **many** invoices | 1 to * |
| Invoice ═══ InvoiceItem | One invoice can have **many** line items | 1 to * |

The role name (e.g., `invoices`, `items`) is how you navigate the association in code.

**Navigating forward** (one → many) gives you a **list**:

```pascal
MyCustomer.Invoices          // all invoices for this customer (a list)
MyCustomer.Invoices[0]       // the first invoice
MyCustomer.Invoices.Count    // how many invoices

MyInvoice.Items              // all line items for this invoice
MyInvoice.Items[2]           // the third item
```

**Navigating backward** (many → one) gives you a **single object**:

```pascal
MyInvoice.Customer           // the customer this invoice belongs to
MyInvoice.Customer.Name      // that customer's name

MyItem.Invoice               // the invoice this item belongs to
MyItem.Invoice.InvoiceNumber // that invoice's number
```

**Derived attributes** are prefixed with `/`. Bold calculates them automatically using OCL:

| Attribute | OCL Expression | Meaning |
|-----------|---------------|---------|
| `/TotalAmount` | `items.lineTotal->sum` | Sum of all line item totals |
| `/LineTotal` | `quantity * unitPrice` | This item's quantity × unit price |

You do not set these values — they update automatically when the underlying data changes.

You create this model visually using the **Bold Model Editor** in the Delphi IDE.

![Bold Model Editor with Invoice model](../images/step1-model-editor.png)
*The Bold Model Editor showing the Customer, Invoice, and InvoiceItem classes with their associations*

### Build It Yourself (Recommended)

This is the proper way to learn Bold — create everything from scratch. The `Shared/` folder already contains **completed versions** of all files as a reference. You can compare your work against them at any point.

> **Tip**: If you get stuck, open the corresponding file in `Shared/` to see the working version.

#### 1. Create a New Data Module

1. In Delphi, open the Step 1 project (`examples/Tutorial/Step1_NewProject/Step1.dproj`)
2. Go to **File > New > Other > Delphi Files > Data Module**
3. Save it as `MyInvoiceDataModule.pas` in the step folder (or any location you prefer)
4. Drop a `TBoldModel` component from the Bold tab of the Component Palette onto the data module
5. Double-click `BoldModel1` to open the **Bold Model Editor**

If you cannot find the Bold tab, Bold for Delphi may not be installed in your Delphi IDE. See the main README's Troubleshooting section.

#### 2. Create the Customer Class

In the Model Editor:

1. Right-click in the main area and select "New Class"
2. Name it `Customer`
3. Add these attributes:
   - Name (Type: String)
   - Address (Type: String)
   - City (Type: String)
   - Phone (Type: String)
   - Email (Type: String)

To add an attribute:
- Right-click the Customer class and select "New Attribute"
- Type the name and select the type from the dropdown

#### 3. Create the Invoice Class

1. Right-click and select "New Class"
2. Name it `Invoice`
3. Add these attributes:
   - InvoiceNumber (Type: Integer)
   - InvoiceDate (Type: Date)
   - DueDate (Type: Date)
   - IsPaid (Type: Boolean)

Do NOT add a TotalAmount attribute yet. We will make it a derived (computed) attribute in Step 4.

#### 4. Create the InvoiceItem Class

1. Right-click and select "New Class"
2. Name it `InvoiceItem`
3. Add these attributes:
   - Description (Type: String)
   - Quantity (Type: Integer)
   - UnitPrice (Type: Currency)

Do NOT add LineTotal yet — that will also be derived in Step 4.

#### 5. Create Associations (Relationships)

Associations link classes together. Create two:

**Association 1: Customer 1-to-Many Invoices**
- Select the Customer class, right-click, "New Association"
- Target: Invoice
- Role name (from Customer): `invoices` (one customer has many invoices)
- Role name (from Invoice): `customer` (one invoice belongs to one customer)
- Multiplicity: Customer 1, Invoice *

**Association 2: Invoice 1-to-Many InvoiceItems**
- Select the Invoice class, right-click, "New Association"
- Target: InvoiceItem
- Role name (from Invoice): `items`
- Role name (from InvoiceItem): `invoice`
- Multiplicity: Invoice 1, InvoiceItem *

#### 6. Generate Code

Once your model is complete:

1. In the Model Editor, go to **File > Generate Code**
2. Select the target folder where you want the generated files
3. Choose your Delphi version from the dropdown
4. Click "Generate"

This creates:
- `InvoiceClasses.pas` — Your domain classes (TCustomer, TInvoice, TInvoiceItem)
- `InvoiceClasses_Interface.inc` — Class declarations and properties

Save the model. You now have a Bold model definition!

**Compare your result**: Open `Shared/InvoiceClasses.pas` and `Shared/InvoiceClasses_Interface.inc` to see the reference versions.

### Use the Pre-Built Files (Quick Start)

If you prefer to skip model creation and jump straight to understanding how Bold works, the `Shared/` folder already contains everything ready to compile and run:

- `InvoiceDataModule.pas` + `.dfm` — The data module with model and persistence
- `InvoiceClasses.pas` + `InvoiceClasses_Interface.inc` — Generated business classes
- `InvoiceModel.bld` — The model definition file

The project is already wired to use these files. Just build and run.

## Setting Up the Data Module

The data module (`InvoiceDataModule.pas`) is where you set up the BoldSystem, persistence, and database connection. Let's understand what each component does.

### The Components

![Data Module with Bold components](../images/step1-datamodule.png)
*The InvoiceDataModule showing the Bold components wired together: BoldModel, SystemTypeInfoHandle, SystemHandle, FireDAC connection, and persistence*

#### TBoldModel
```
BoldModel1: TBoldModel
```

This component holds your UML model definition. It loads the model file you created in the Model Editor. Bold uses this to know the structure of your classes (what attributes they have, what associations exist).

#### TBoldSystemTypeInfoHandle
```
BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle
```

This component connects the BoldModel to the BoldSystem. It provides "runtime type information" — it tells the BoldSystem what the classes look like at runtime. When Bold creates an object, it looks at this handle to know what attributes and methods the class has.

#### TBoldSystemHandle
```
BoldSystemHandle1: TBoldSystemHandle
  TypeInfoHandle: BoldSystemTypeInfoHandle1
```

The BoldSystemHandle creates and manages the **BoldSystem**. The BoldSystem is the heart of Bold:

- It is an **in-memory object database**
- When you load a customer from the SQL database, it lives in the BoldSystem
- When you create a new customer, it is added to the BoldSystem
- When you modify a customer's name, the BoldSystem tracks that the object is "dirty"
- When you call `UpdateDatabase`, the BoldSystem writes all dirty objects back to the SQL database

**Key concept**: The BoldSystem is not the SQL database. It is a separate in-memory pool of objects. Bold syncs between the BoldSystem and the database.

#### TFDConnection (FireDAC)
```
FFDConnection: TFDConnection
  DriverID: SQLite
  Database: Step1.db
```

This opens a connection to SQLite. FireDAC is Delphi's multi-database driver layer. Bold uses FireDAC to talk to the database.

#### TBoldDatabaseAdapterFireDAC
```
FFireDACAdapter: TBoldDatabaseAdapterFireDAC
  Connection: FFDConnection
```

This adapter tells Bold how to use FireDAC. It translates Bold's persistence requests into FireDAC SQL calls.

#### TBoldPersistenceHandleDB
```
FPersistenceHandleDB: TBoldPersistenceHandleDB
  DatabaseAdapter: FFireDACAdapter
  SystemHandle: BoldSystemHandle1
```

This component connects the BoldSystem to the database. When you call `UpdateDatabase`:

1. The persistence handle asks the BoldSystem which objects are dirty
2. It generates SQL INSERT/UPDATE/DELETE statements
3. It executes them via FireDAC
4. The database is updated

#### TBoldActivateSystemAction
```
BoldActivateSystemAction1: TBoldActivateSystemAction
  SystemHandle: BoldSystemHandle1
```

This action opens and closes the BoldSystem. Calling `Execute` on this action:

1. Loads the model from the database
2. Initializes the BoldSystem
3. Fires `OnSystemOpened` event

Closing calls `OnSystemClosed`.

### Putting It Together

In the data module's `Create` method:

```pascal
procedure TInvoiceDataModule.DataModuleCreate(Sender: TObject);
begin
  SetupPersistence;
end;
```

`SetupPersistence` creates and wires all the persistence components above. Later, when the system is activated (via `BoldActivateSystemAction1`), `EnsureDatabaseExists` checks if Bold's schema tables exist. If not, Bold creates the database schema automatically.

### Persistence: How Objects Get to the Database

Here is the flow:

1. **Create**: You create a new object: `Customer := TCustomer.Create(BoldSystem.ObjectSpace);`
2. **Modify**: You change attributes: `Customer.Name := 'Acme Corp';`
3. **Track**: The BoldSystem tracks that Customer is "dirty"
4. **Save**: You call `BoldSystem.UpdateDatabase;`
5. **SQL**: Bold generates: `INSERT INTO Customers (Name, Address, ...) VALUES ('Acme Corp', ...);`
6. **Commit**: The SQL is executed in a transaction
7. **Reload**: The BoldSystem resets its dirty flags

**Important**: Changes in memory (the BoldSystem) are invisible to the database until you call `UpdateDatabase`. If you modify an object and then restart the application without saving, the changes are lost.

## The Form

Step 1's form (`Step1Form.pas`) is minimal. It just demonstrates that the system opens:

```pascal
procedure TfrmStep1.FormCreate(Sender: TObject);
begin
  dmInvoice.BoldActivateSystemAction1.OnSystemOpened := HandleSystemOpened;
  dmInvoice.BoldActivateSystemAction1.OnSystemClosed := HandleSystemClosed;
  dmInvoice.OpenSystem;  // Open the BoldSystem
end;

procedure TfrmStep1.HandleSystemOpened(Sender: TObject);
begin
  lblBoldStatus.Caption := 'Bold System: OPEN';
end;
```

When you run the application:

1. The form calls `OpenSystem`
2. The BoldSystem loads the model and initializes
3. The `OnSystemOpened` event fires, and the label updates
4. The form displays the database filename and status

That is it! Step 1 is just foundation-building. The real UI work starts in Step 2.

## Key Concepts

### BoldSystem = In-Memory Object Database

Think of the BoldSystem as a cache:

- **Before**: You query the SQL database directly with SQL queries
- **Now**: You load objects into the BoldSystem once, then work with them in memory
- **Benefit**: Faster access, automatic change tracking, type safety

### Persistence = Automatic

You do not write SQL. You manipulate objects, and Bold handles the database:

```pascal
Customer.Name := 'New Name';        // Change object in memory
BoldSystem.UpdateDatabase;          // Bold generates SQL and updates database
```

### Model = Source of Truth

The UML model you create in the Model Editor is the source of truth for your application's structure. When you generate code, Bold creates Delphi classes that match the model.

If you add an attribute to the model, you regenerate, and the Delphi class gets the new property. If you modify the model, regenerate to update the Delphi classes.

## Try It Yourself

### Exercise 1: Add a New Attribute

1. Open the Bold model in the Model Editor (double-click `BoldModel1` in InvoiceDataModule)
2. Select the Customer class
3. Right-click and "New Attribute"
4. Name it `Website` with type String
5. Go to File > Generate Code
6. Rebuild the project

Now the TCustomer class has a new `Website` property. Try reading it in the form:

```pascal
ShowMessage(dmInvoice.BoldSystemHandle1.System.AllInstances[0].ClassOfObject.ClassName);
```

### Exercise 2: Change the Database

By default, the application stores data in `Step1.db` (next to the executable).

1. Open `Step1.ini` (next to the executable)
2. Change the filename: `Database=MyData.db`
3. Restart the application

Bold creates a new database file. The old `Step1.db` is unchanged. This shows that the database location is configurable.

### Exercise 3: Inspect the Database

The SQLite database file can be inspected with any SQLite viewer (e.g., [DB Browser for SQLite](https://sqlitebrowser.org/)).

1. Stop the application
2. Open `Step1.db` in DB Browser
3. Look at the schema: Bold created tables for Customer, Invoice, InvoiceItem
4. The table columns match the attributes you defined in the model

This shows how Bold auto-creates the database schema from the model.

![SQLite database with Bold tables](../images/step1-sqlite-tables.png)
*DB Browser for SQLite showing the auto-created tables matching the Bold model*

## Next Step

Now that the foundation is in place, you are ready to build a real UI!

Proceed to [`Step2_CustomerCRUD/README.md`](../Step2_CustomerCRUD/README.md) to learn how to create, read, update, and delete customers through a user interface.

---

**Key Takeaways**:
- Bold is an ORM that maps objects to database tables
- The BoldModel defines your class structure
- The BoldSystem is an in-memory object cache
- The data module wires everything together
- Database persistence is automatic and transaction-safe
