# Step 5: Inheritance and VAT

In this step you learn two object-oriented concepts as Bold applies them:

1. **Inheritance**: "is-a" relationships (a Company *is a* Customer)
2. **Lookup classes**: reference data that other objects point to (VAT rates)

By the end, you know how Bold stores a class hierarchy in the database, how to create objects of a given subclass,
and how a derived attribute navigates an association.

## What You Will Learn

- How to model inheritance in a Bold model
- How Bold stores a class hierarchy in database tables
- Why `Customer.allInstances` returns both Company and PrivatePerson objects
- How to create objects of a given subclass with `CreateNewObjectByExpressionName`
- How a lookup class works with an association
- How a derived attribute navigates an association (`VATAmount = lineTotal * vatRate.percentage / 100`)
- How a `TBoldGrid` column offers a drop-down list through `LookUpProperties`

## The Extended Model

Step 5 extends the model of Steps 1 to 4 with inheritance and a lookup class:

```
 ╔═══════════════════╗
 ║  Customer (base)  ║
 ╠═══════════════════╣
 ║ Name       String ║
 ║ Address    String ║
 ║ PostAddress String║
 ║ City       String ║        ╔═════════════════════╗
 ║ Country    String ╠0..1═*══╣      Invoice        ║
 ╚════════╤══════════╝        ╠═════════════════════╣
     ┌────┴────┐              ║ InvoiceNumber  Int  ║
     │         │              ║ InvoiceDate   Date  ║
 ╔═══╧═════╗ ╔╧════════════╗ ║ DueDate       Date  ║
 ║ Company ║ ║PrivatePerson║ ║ IsPaid     Boolean  ║
 ╠═════════╣ ╠═════════════╣ ║ /TotalAmt  Currency ║
 ║ VATNum  ║ ║ (no extra   ║ ╚══════════╤══════════╝
 ║ VATExem ║ ║  attributes)║       0..1 │   *
 ╚═════════╝ ╚═════════════╝  ╔═════════╧═══════════╗
                               ║    InvoiceItem      ║
 ╔═══════════════════╗         ╠═════════════════════╣
 ║     VATRate       ║         ║ Description  String ║
 ╠═══════════════════╣  0..1   ║ Quantity       Int  ║
 ║ Name       String ╠════════╣ UnitPrice  Currency ║
 ║ Percentage  Float ║vatRate  ║ /LineTotal Currency ║
 ╚═══════════════════╝         ║ /VATAmount Currency ║
                               ╚═════════════════════╝
```

### What Changed from Steps 1 to 4

| Change | Before (Steps 1 to 4) | After (Step 5) |
|--------|-----------------------|----------------|
| Customer | Concrete class with Phone, Email | Abstract base class with PostAddress, Country |
| Subclasses | None | Company (VATNumber, VATExempt), PrivatePerson |
| VATRate | Did not exist | New lookup class with Name, Percentage |
| InvoiceItem.VATRate | Did not exist | New association to VATRate (0..1) |
| InvoiceItem.VATAmount | Did not exist | New derived attribute: `lineTotal * vatRate.percentage / 100` |

## Concept 1: Inheritance in Bold

### What is Inheritance?

Inheritance models "is-a" relationships. A Company **is a** Customer, and so is a PrivatePerson. Both have the
attributes of Customer (Name, Address, City, Country), and Company adds its own (VATNumber, VATExempt).

### How Bold Stores Inheritance in the Database

By default every class gets its own table (the class's table mapping is `Own`), and a table holds only the
attributes its class defines:

```
Customer table
┌────────┬───────────┬──────┬─────────┬─────────────┬──────┬─────────┐
│BOLD_ID │ BOLD_TYPE │ Name │ Address │ PostAddress │ City │ Country │
├────────┼───────────┼──────┼─────────┼─────────────┼──────┼─────────┤
│ 101    │ (Company) │ Acme │ 123 St  │ ...         │ NYC  │ US      │
│ 102    │ (Person)  │ John │ 456 Ave │ ...         │ LA   │ US      │
└────────┴───────────┴──────┴─────────┴─────────────┴──────┴─────────┘

Company table
┌────────┬───────────┬───────────┬───────────┐
│BOLD_ID │ BOLD_TYPE │ VATNumber │ VATExempt │
├────────┼───────────┼───────────┼───────────┤
│ 101    │ (Company) │ US123456  │ False     │
└────────┴───────────┴───────────┴───────────┘
```

- Every table has the columns `BOLD_ID`, the object's identity, and `BOLD_TYPE`, a number for its class. Bold's
  table `BOLD_TYPE` lists the class names.
- A company is one row in `Customer` and one in `Company`, with the same `BOLD_ID`.

The **Table mapping** of a class in the model editor (tagged value `TableMapping`) changes this. With `Parent`, a
class stores its attributes in its superclass's table, so a whole hierarchy can live in one table.

### Polymorphic Queries with OCL

OCL queries follow the class hierarchy:

```
Customer.allInstances                    → all customers (Company and PrivatePerson)
Company.allInstances                     → only companies
PrivatePerson.allInstances               → only private persons
Customer.allInstances->size              → number of customers
Company.allInstances->size               → number of companies
```

Bold knows the class hierarchy: `Customer.allInstances` returns every object whose class is Customer or a subclass
of it.

### Type Checks in OCL

```
customer.oclIsTypeOf(Company)            → true if the class is exactly Company
customer.oclIsKindOf(Customer)           → true if the class is Customer or a subclass of it
customer.oclAsType(Company).vatNumber    → cast to Company to reach a Company attribute
```

### Creating Objects of a Subclass

Customer is abstract in this model, so you cannot create a Customer; you create a Company or a PrivatePerson. The
form does it by expression name:

```pascal
// Create a Company
dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Company');

// Create a PrivatePerson
dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('PrivatePerson');
```

With `InvoiceClasses` in the `uses` clause, the generated class does the same:

```pascal
TCompany.Create(dmStep5.BoldSystemHandle1.System);
```

The new object appears in:

- `Customer.allInstances`, because a Company is a Customer
- `Company.allInstances`, because it is a Company

### Why Not the Navigator's Insert Button?

The Insert button of a `TBoldNavigator` creates an object of the list's class. For `Customer.allInstances` that is
Customer, and Bold refuses to create an object of an abstract class. So the form has a button for each concrete
class instead.

## Concept 2: Lookup Classes (VATRate)

### What is a Lookup Class?

A lookup class is a simple reference table. `VATRate` has two attributes:

- **Name**: a readable label, such as "Standard 24%", "Reduced 14%" or "Zero"
- **Percentage**: the rate as a number, such as 24.0, 14.0 or 0.0

Invoice items **point to** a VATRate through an association. This is Bold's version of a foreign key to a lookup
table in SQL.

### The InvoiceItem-VATRate Association

```
InvoiceItem  * ──────── 0..1  VATRate
     invoiceItems     vatRate
```

An InvoiceItem has at most one VATRate (`0..1`); a VATRate can be used by many InvoiceItems. When you assign a
VATRate to an item, the item's VATAmount follows.

### The VATAmount Derived Attribute

```
VATAmount = lineTotal * vatRate.percentage / 100
```

This expression **navigates the association**: it reads `vatRate.percentage` from the linked VATRate. VATAmount
follows when you:

- change the quantity or the unit price (which changes lineTotal),
- link another VATRate, or
- change the percentage of the linked VATRate.

### A Drop-Down List in the Grid

The Items grid has a "VAT Rate" column that offers a drop-down list. The grid column's `LookUpProperties` set it
up:

```
Column.BoldProperties.Expression   = 'vatRate'
Column.LookUpProperties.Expression = 'VATRate.allInstances'
```

`LookUpProperties.Expression` gives the choices, all VATRate objects. The user picks one, and Bold sets the
association.

## Building Step 5

### Running the Finished Step

Open `Step5.dproj` in Delphi, choose the Win64 target platform, and press F9.

### Why Step 5 Has Its Own Data Module

Steps 1 to 4 share the model in `Shared/InvoiceDataModule.dfm`. Step 5 has new classes (Company, PrivatePerson,
VATRate) and a new association. The model is stored in the data module's `.dfm`, so Step 5 has its own data
module, `Step5DataModule`, and its own generated `InvoiceClasses.pas`. The Pascal code of the data module is the
same as in `Shared/`.

### Extending the Model Yourself

Start from a copy of your project of Step 4, and in the model editor:

1. Select the Customer class and check **Abstract**. Delete its attributes Phone and Email, and add PostAddress
   and Country (String).
2. Right-click Customer and choose **Add Subclass**. Rename the new class to `Company` and add VATNumber (String)
   and VATExempt (Boolean).
3. Add a second subclass of Customer, `PrivatePerson`, with no attributes.
4. Add the class `VATRate` with Name (String) and Percentage (Float).
5. Add an association `InvoiceItemVATRate` with these ends:

   | End name | Class | Multiplicity |
   |----------|-------|--------------|
   | `VATRate` | VATRate | `0..1` |
   | `InvoiceItems` | InvoiceItem | `*` |

6. Add the attribute `VATAmount` (Currency) to InvoiceItem: check **Derived**, clear **Persistent**, and set
   **Derivation OCL** to `lineTotal * vatRate.percentage / 100`.
7. Generate the code (**Tools > Generate Code**), and delete the old database file: the model has new tables.

Compare your result with `Step5DataModule.dfm` and `InvoiceClasses_Interface.inc` in the step folder.

### First Run: Setting Up Data

On the first run the database is empty. A suggested order:

1. **Create the VAT rates first**; the invoice items need them.
   - Click **Insert** in the VAT Rates navigator.
   - Add "Standard 24%" with Percentage 24.0.
   - Add "Reduced 14%" with Percentage 14.0.
   - Add "Zero" with Percentage 0.0.

2. **Create customers**:
   - Click **New Company** and type a Name and a City in the grid.
   - Click **New Person** and do the same.
   - The "Type" column shows "Company" or "PrivatePerson".

   The customer grid shows only Type, Name and City. To edit Address, Country or VATNumber, add columns or edit
   fields for them.

3. **Create invoices and items**:
   - Select a customer and click **Insert** in the Invoices navigator.
   - Select the invoice and click **Insert** in the Items navigator.
   - Type a Description, a Quantity and a Unit Price.
   - Pick a VAT rate from the drop-down list.
   - Watch Line Total and VAT Amount follow.

4. **Save**: click **Save Changes** to write everything to SQLite.

## Key Code Patterns

### Creating Objects by Class Name

```pascal
// By expression name; the form needs no reference to the generated classes:
dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Company');

// With the generated class:
TCompany.Create(dmStep5.BoldSystemHandle1.System);
```

### Polymorphic Delete

```pascal
// CurrentBoldObject is a TBoldObject; the object itself is a TCompany or a TPrivatePerson.
// Delete works whatever the concrete class:
lhaCustomers.CurrentBoldObject.Delete;
```

### Derived Attributes That Navigate Associations

```
InvoiceItem.VATAmount = lineTotal * vatRate.percentage / 100
```

Until an item has a VAT rate, `vatRate` is nil and `vatRate.percentage` has no value. Pick a VAT rate for each item.

## Handle Chain

```
BoldSystemHandle1
     |
     +--> lhaCustomers       Expression='Customer.allInstances'
     |         |               (returns both Company and PrivatePerson)
     |         |
     |         +--> lhaInvoices  Expression='invoices'
     |                   |
     |                   +--> lhaItems  Expression='items'
     |
     +--> lhaVATRates        Expression='VATRate.allInstances'
     |
     +--> lhaCompanies       Expression='Company.allInstances'
```

`lhaCompanies` is there for your own use, for example a grid of companies only or a company count. The form shows
all customers through `lhaCustomers`.

## OCL Reference for Inheritance

| Expression | Returns |
|------------|---------|
| `Customer.allInstances` | All customers (both classes) |
| `Company.allInstances` | Only companies |
| `PrivatePerson.allInstances` | Only private persons |
| `oclType.name` | The class name as a string ("Company" or "PrivatePerson") |
| `oclIsTypeOf(Company)` | True if the class is exactly Company |
| `oclIsKindOf(Customer)` | True if the class is Customer or a subclass of it |
| `oclAsType(Company).vatNumber` | Cast to Company, then read VATNumber |
| `Company.allInstances->select(vatExempt)` | All VAT-exempt companies |

The form's customer count label combines several of them:

```
('Customers: ' + Customer.allInstances->size.asString +
 '  (Companies: ' + Company.allInstances->size.asString +
 ', Persons: ' + PrivatePerson.allInstances->size.asString + ')')
```

## Try It Yourself

### Exercise 1: Invoice Total of Companies

Add a `TBoldLabel` with the total of all invoices of companies:

```
Company.allInstances->collect(invoices)->collect(totalAmount)->sum.asString
```

### Exercise 2: VAT Summary

Add a `TBoldLabel` with the total VAT of all items:

```
InvoiceItem.allInstances->collect(vatAmount)->sum.asString
```

### Exercise 3: Conditional Display

Show the VAT number for companies only. Add a column to the customer grid with this expression:

```
if oclIsTypeOf(Company) then oclAsType(Company).vatNumber else '' endif
```

## Common Pitfalls

### You Cannot Create an Object of an Abstract Class

Customer is abstract, so **Insert** of a `TBoldNavigator` on `Customer.allInstances` fails. Create each concrete
class with `CreateNewObjectByExpressionName` or its generated class instead.

### Navigating to Nothing in a Derived Attribute

An InvoiceItem without a VATRate has no `vatRate.percentage` to calculate VATAmount from. Assign a VAT rate to
each item.

### Database Schema Changes

Step 5 has its own database file, `Step5.db`. The databases of Steps 1 to 4 are separate and stay as they are. To
start Step 5 again, delete `Step5.db` and run again.

## What's Next

You have completed all five steps. You now understand:

1. **Step 1**: the Bold model and database persistence
2. **Step 2**: CRUD UIs with handles and grids
3. **Step 3**: associations and master-detail relationships
4. **Step 4**: OCL queries for filtering and aggregation
5. **Step 5**: class hierarchies and lookup classes

These cover the core of Bold for Delphi. For more, explore:

- **Undo and redo**: Bold's built-in undo mechanism (`TBoldUndoHandler`)
- **Constraints**: validation rules in the model
- **Subscriptions**: how Bold refreshes the UI when data changes
- **Database evolution**: changing the database schema as your model evolves
