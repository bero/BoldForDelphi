# Bold for Delphi Invoice Tutorial

Welcome! This tutorial teaches you how to build a real-world application using Bold for Delphi, from absolute zero. No prior knowledge of Bold, ORM, or object-oriented databases required.

## What You Will Learn

This five-step tutorial guides you through building an Invoice Management System:

1. **Step 1**: Create a model and set up a Bold project
2. **Step 2**: Build a Customer CRUD interface (Create, Read, Update, Delete)
3. **Step 3**: Add Invoices and learn about relationships between objects
4. **Step 4**: Write OCL expressions to query and calculate data
5. **Step 5**: Model inheritance (Company/PrivatePerson) and lookup tables (VAT rates)

By the end, you will understand:

- How Bold models represent real-world concepts as objects
- How the BoldSystem acts as an in-memory object database
- How to connect UI components to Bold objects without writing data-binding code
- How OCL expressions replace SQL for querying objects
- How Bold automatically tracks changes and writes to the database
- How inheritance maps to the database and enables polymorphic queries
- How lookup/reference classes work with derived attributes that navigate associations

## Prerequisites

- **Delphi 12+** with VCL (Visual Component Library)
- **Bold for Delphi** installed and compiled in your Delphi IDE
- **SQLite** (included in FireDAC, no separate install needed)
- Basic Delphi knowledge (how to create a form, drop components on a form, add event handlers)

If you have never used Bold before, that is fine. This tutorial expects no prior ORM or Bold experience.

## The Domain Model

The tutorial teaches you to build this simple three-class model:

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

![Tutorial domain model in Bold Model Editor](images/step1-model-editor.png)
*The three-class Invoice model as it appears in the Bold Model Editor*

**Reading the diagram:**

- The `═══` lines are **associations** (relationships). `1` and `*` show multiplicity: one customer has many (`*`) invoices
- The role names (`invoices`, `items`) are how you navigate in code
- `/` prefix marks **derived attributes** — Bold calculates them automatically via OCL

**Navigating in code** — forward (one→many) returns a list, backward (many→one) returns a single object:

```pascal
MyCustomer.Invoices          // list of invoices for this customer
MyCustomer.Invoices[0]       // first invoice
MyInvoice.Customer           // the customer this invoice belongs to
MyInvoice.Customer.Name      // that customer's name
MyInvoice.Items              // list of line items
MyItem.Invoice               // the invoice this item belongs to
```

## Building the Tutorial Steps

Each step is a standalone Delphi project. Build them using the PowerShell script:

```powershell
C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1 -Projectfile "examples\Tutorial\Step1_NewProject\Step1.dproj" -VerboseOutPut
```

Replace `Step1_NewProject\Step1.dproj` with `Step2_CustomerCRUD\Step2.dproj`, `Step3_Invoices\Step3.dproj`, `Step4_OclExpressions\Step4.dproj`, or `Step5_InheritanceVAT\Step5.dproj` as needed.

If the script is not available on your system, you can build manually in the Delphi IDE:
1. Open the .dproj file in Delphi
2. Press F9 to compile and run

## Shared Files

All steps share these files in the `Shared/` folder:

- **InvoiceClasses.pas** and **InvoiceClasses_Interface.inc** — Auto-generated Bold domain classes (Customer, Invoice, InvoiceItem) and their attributes/associations
- **InvoiceDataModule.pas** — The BoldSystem, persistence, and database connection setup

The tutorial teaches you to create the model that generates InvoiceClasses. If you prefer to skip the model creation step, you can use the pre-built classes in `Shared/` and focus on learning how to build UIs on top of Bold.

## Database Setup

Each step uses its own SQLite database file:

- Step 1: `Step1.db`
- Step 2: `Step2.db`
- Step 3: `Step3.db`
- Step 4: `Step4.db`
- Step 5: `Step5.db`

These files are in the same directory as the executable and are created automatically on first run. You can delete them to start fresh. The database schema is created automatically from the Bold model.

## Step Overview

### Step 1: New Project Setup
Learn what a BoldSystem is, how to create a UML model, how persistence works, and how Bold auto-creates database tables. This step is mostly setup; the form just shows that the system opens and connects to the database.

**Key Components**: BoldModel, BoldSystem, BoldSystemHandle, Persistence, Database

**Read**: [`Step1_NewProject/README.md`](Step1_NewProject/README.md)

### Step 2: Customer CRUD
Build a complete UI for managing customers. Learn about TBoldListHandle, TBoldGrid, TBoldNavigator, and TBoldEdit. Understand how Bold objects stay synchronized across UI components.

**Key Components**: TBoldListHandle, TBoldGrid, TBoldNavigator, TBoldEdit

**Read**: [`Step2_CustomerCRUD/README.md`](Step2_CustomerCRUD/README.md)

![Step 2 Customer CRUD](images/step2-form.png)
*Step 2: A complete customer management UI built with zero SQL*

### Step 3: Invoices and Associations
Add Invoices and learn how Bold models relationships between objects. Build a master-detail UI using handle chaining: select a customer to see their invoices, select an invoice to see its items.

**Key Components**: Handle chaining, Master-Detail relationships, Derived attributes

**Read**: [`Step3_Invoices/README.md`](Step3_Invoices/README.md)

### Step 4: OCL Expressions
Write Object Constraint Language (OCL) expressions to filter, aggregate, and calculate data. Learn BoldLabel for dynamic read-only text that updates automatically.

**Key Components**: OCL select, collect, sum, BoldLabel, Dynamic expressions

**Read**: [`Step4_OclExpressions/README.md`](Step4_OclExpressions/README.md)

![Step 4 OCL Dashboard](images/step4-dashboard.png)
*Step 4: Live-updating dashboard with OCL-driven labels and filtered lists*

### Step 5: Inheritance & VAT
Extend the model with class inheritance (Customer becomes a base class for Company and PrivatePerson) and a lookup table (VATRate). Learn how Bold maps inheritance to a single database table, how to create specific subtypes, and how derived attributes can navigate associations to calculate VAT amounts.

**Key Concepts**: Inheritance, Table-Per-Hierarchy, Abstract classes, Lookup classes, Association navigation in derived attributes

**Read**: [`Step5_InheritanceVAT/README.md`](Step5_InheritanceVAT/README.md)

## Important Notes

### Two Ways to Learn

**Build from scratch (recommended)**: Each step's README guides you through creating the model, data module, and forms yourself using the Bold Model Editor and Delphi IDE. This is the best way to learn — you understand every piece because you built it.

**Use the pre-built files as reference**: The `Shared/` folder contains completed versions of the model, generated classes, and data module. Each step folder contains the completed form. These serve as the **answer key** — compare your work against them at any point, or use them directly if you want to skip ahead.

If you are new to Bold, we strongly recommend building from scratch. The Model Editor instructions teach the correct mental model: design your objects in the UML editor, generate code, then build UI.

## What Happens Inside Bold

When you interact with a UI component (like TBoldGrid):

1. You type in a cell or click a button
2. The component updates a Bold object in memory
3. Bold tracks that the object is "dirty" (has unsaved changes)
4. When you call `UpdateDatabase`, Bold generates SQL INSERT/UPDATE statements
5. The database is updated in a single transaction

From the Delphi side, you don't write SQL. You just manipulate objects like normal Delphi instances, and Bold handles all the database persistence.

## What's Next

After completing all five steps, you will be ready to:

- Build your own applications from scratch using the Model Editor
- Read the Bold wiki and example projects
- Understand OCL well enough to write complex queries
- Handle associations, inheritance, and constraints in your models
- Use Bold's undo/redo and subscription system for advanced features

## Troubleshooting

### Database file not found or locked
Delete the .db file (e.g., Step1.db) and re-run. Bold will create a fresh database.

### "Unit 'Bold...' not found"
Make sure Bold for Delphi packages are installed in your Delphi IDE. Open `packages/Delphi12.3/dclBold.dpk` (or the appropriate version) and click "Install".

### Form or model file missing
Each step has its own .dproj file and form. Make sure you are opening the correct project. Do not mix Step 1 and Step 2 source files.

### SQLite database locked during execution
Close all instances of the application before rebuilding. SQLite does not allow concurrent writers.

## Resources

- **Bold for Delphi Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi
- **Bold Blog**: http://boldfordelphi.blogspot.com/
- **Discord Community**: https://discord.gg/C6frzsn
- **This Project on GitHub**: https://github.com/BoldForDelphi/BoldForDelphi

---

Ready to start? Go to [`Step1_NewProject/README.md`](Step1_NewProject/README.md).
