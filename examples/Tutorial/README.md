# Bold for Delphi Invoice Tutorial

This tutorial builds an invoicing application with Bold for Delphi, in five steps, from an empty project. You need
no earlier knowledge of Bold, ORM or object-oriented databases.

The tutorial is a VCL application for Delphi on Windows. It uses the Bold-aware VCL controls (`TBoldGrid`,
`TBoldEdit`, `TBoldNavigator`, `TBoldLabel`), which exist only for the VCL. For FireMonkey (FMX) or Lazarus (LCL),
which bind through `TBoldDataSet` instead, start from the Invoicing example's
[FMX](../Compound/Invoicing/FMX/README.md) or [LCL](../Compound/Invoicing/LCL/README.md) client.

## What You Will Learn

1. **Step 1**: Create a model and set up a Bold project
2. **Step 2**: Build a Customer CRUD interface (Create, Read, Update, Delete)
3. **Step 3**: Add invoices and learn about associations between objects
4. **Step 4**: Write OCL expressions to query and calculate data
5. **Step 5**: Model inheritance (Company and PrivatePerson) and a lookup class (VAT rates)

By the end, you will understand:

- How a Bold model represents real-world concepts as classes
- How the object space (the `TBoldSystem`) holds your objects in memory
- How to connect UI components to Bold objects without writing data-binding code
- How OCL expressions replace SQL for querying objects
- How Bold tracks changes and writes them to the database
- How inheritance maps to the database and how OCL queries a class hierarchy
- How a derived attribute can navigate an association to a lookup class

## Prerequisites

- **Delphi 12.3 Athens or 13.1 Florence** on Windows, with the Bold design package `dclBold` installed
  (`packages/Delphi12.3` or `packages/Delphi13`). See [Installation](../../docs/getting-started/installation.md).
- **SQLite** through FireDAC. FireDAC brings the SQLite driver, so there is nothing else to install.
- Basic Delphi knowledge: create a form, drop components on it, add event handlers.

## The Domain Model

Steps 1 to 4 use this three-class model:

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

**Reading the diagram:**

- The `═══` lines are **associations**. The multiplicities say that a customer has any number (`*`) of invoices
  and an invoice has at most one (`0..1`) customer.
- The role names (`invoices`, `items`) are how you navigate from one class to the other.
- The `/` prefix marks a **derived attribute**: Bold calculates it from an OCL expression.

**Navigating in code**: the "many" end gives a list, the "one" end a single object:

```pascal
MyCustomer.Invoices          // list of invoices for this customer
MyCustomer.Invoices[0]       // first invoice
MyInvoice.Customer           // the customer this invoice belongs to
MyInvoice.Customer.Name      // that customer's name
MyInvoice.Items              // list of line items
MyItem.Invoice               // the invoice this item belongs to
```

## Running the Steps

Each step is a Delphi project of its own, with the finished form. To run one:

1. Open the step's `.dproj` in Delphi, for example `Step1_NewProject\Step1.dproj`.
2. Choose the **Win64** target platform. Win32 still compiles but is no longer tested.
3. Press F9.

The projects find Bold's sources through relative search paths (`..\..\..\Source\...`), so keep them where they
are in the repository.

## Two Ways to Learn

**Build it yourself (recommended)**: each step's README tells you how to create the model, the data module and the
forms yourself, in the Bold model editor and the Delphi IDE. You understand every piece because you built it.
Design the classes in the model editor, generate the code, then build the UI.

**Use the finished files as the answer key**: the `Shared/` folder holds the finished model, generated classes and
data module of Steps 1 to 4, and each step folder holds the finished form. Compare your work with them at any point,
or run them to skip ahead.

## Shared Files

Steps 1 to 4 share these files in `Shared/`:

- **InvoiceModel.bld**: the model as a `.bld` file
- **InvoiceClasses.pas** and **InvoiceClasses_Interface.inc**: the business classes Bold generated from the model
  (`TCustomer`, `TInvoice`, `TInvoiceItem`)
- **InvoiceDataModule.pas** and **.dfm**: the model component, the system handle, the persistence and the database
  connection

Step 5 extends the model, so it has its own data module (`Step5DataModule`) and its own generated
`InvoiceClasses.pas`.

## Database Setup

Each step uses its own SQLite database file, `Step1.db` to `Step5.db`. The file name comes from the step's `.ini`
file (`[SQLite] Database=`). Both lie in the step folder, next to the executable. On the first run the data module
creates the file and Bold creates the tables from the model. Delete the `.db` file to start again with an empty
database.

## Step Overview

### Step 1: New Project Setup

What the object space is, how to create a UML model, how persistence works, and how Bold creates the database
tables. This step is mostly setup; its form only shows that the system opens and connects to the database.

**Key components**: `TBoldModel`, `TBoldSystemTypeInfoHandle`, `TBoldSystemHandle`, `TBoldPersistenceHandleDB`,
`TBoldDatabaseAdapterFireDAC`

**Read**: [`Step1_NewProject/README.md`](Step1_NewProject/README.md)

### Step 2: Customer CRUD

A complete UI for managing customers with `TBoldListHandle`, `TBoldGrid`, `TBoldNavigator` and `TBoldEdit`, and
how Bold keeps the controls in step with the objects.

**Key components**: `TBoldListHandle`, `TBoldGrid`, `TBoldNavigator`, `TBoldEdit`

**Read**: [`Step2_CustomerCRUD/README.md`](Step2_CustomerCRUD/README.md)

### Step 3: Invoices and Associations

How Bold models relationships between objects. A master-detail UI through handle chaining: select a customer to see
its invoices, select an invoice to see its items. The first derived attributes.

**Key concepts**: associations, handle chaining, derived attributes

**Read**: [`Step3_Invoices/README.md`](Step3_Invoices/README.md)

### Step 4: OCL Expressions

OCL expressions that filter, aggregate and calculate, and `TBoldLabel` for read-only text that updates by itself.

**Key concepts**: `select`, `collect`, `sum`, `size`, `TBoldLabel`

**Read**: [`Step4_OclExpressions/README.md`](Step4_OclExpressions/README.md)

### Step 5: Inheritance and VAT

The model grows: `Customer` becomes an abstract base class of `Company` and `PrivatePerson`, and a lookup class
`VATRate` arrives. How Bold stores a class hierarchy in the database, how to create objects of a given subclass, and
how a derived attribute navigates an association to calculate the VAT amount.

**Key concepts**: inheritance, abstract classes, lookup classes, association navigation in derived attributes

**Read**: [`Step5_InheritanceVAT/README.md`](Step5_InheritanceVAT/README.md)

## What Happens Inside Bold

When you work with a Bold-aware control such as `TBoldGrid`:

1. You type in a cell or click a button.
2. The control changes a Bold object in memory.
3. Bold marks the object as **dirty** (it has unsaved changes).
4. When you call `UpdateDatabase`, Bold generates the SQL INSERT, UPDATE and DELETE statements.
5. The database is updated in a single transaction.

You do not write SQL. You work with objects like ordinary Delphi instances, and Bold handles the persistence.

## What's Next

After the five steps you can:

- Build your own applications with the model editor
- Read the other examples in this repository
- Write OCL queries of your own
- Model associations, inheritance and constraints
- Explore Bold's undo/redo and subscription system

## Troubleshooting

### "Unit 'Bold...' not found"

The step projects find Bold's units through relative search paths. If you moved a project out of the repository,
or you build a project of your own, put Bold's source folders on its search path, as
[Installation, Step 3](../../docs/getting-started/installation.md#step-3-let-your-projects-find-bolds-source)
describes. Copying the search path of a step project is the easiest way.

### The Bold components are missing from the Tool Palette

Install the design package: open `packages/Delphi12.3/dclBold.dproj` (or `packages/Delphi13/dclBold.dproj` for
Delphi 13), build it, then right-click the package in the Projects pane and choose **Install**. See
[Installation, Step 2](../../docs/getting-started/installation.md#step-2-build-and-install-the-design-time-package).

### Form or model file missing

Each step has its own `.dproj` and form. Make sure you open the right project, and do not mix the source files of
different steps.

### Database file locked, or an error after a model change

Close every running instance of the step before you rebuild it; SQLite allows only one writer at a time. The data
module creates the tables only when the database has none, so after a change to the model, delete the step's `.db`
file and run again.

## Resources

- **Documentation**: https://bero.github.io/BoldForDelphi/
- **Bold for Delphi Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi
- **Bold Blog**: http://boldfordelphi.blogspot.com/
- **Discord**: https://discord.gg/C6frzsn
- **GitHub**: https://github.com/bero/BoldForDelphi

---

Ready to start? Go to [`Step1_NewProject/README.md`](Step1_NewProject/README.md).
