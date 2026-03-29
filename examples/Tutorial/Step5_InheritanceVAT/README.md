# Step 5: Inheritance & VAT

In this step, you will learn two fundamental object-oriented concepts as they apply to Bold for Delphi:

1. **Inheritance** - Modeling "is-a" relationships (a Company *is a* Customer)
2. **Lookup classes** - Reference data that other objects point to (VAT rates)

By the end, you will understand how Bold handles class hierarchies in the database, how to create objects of specific subtypes, and how derived attributes can navigate associations.

## What You Will Learn

- How to model inheritance in a Bold UML model
- How Bold maps inheritance to a single database table (Table-Per-Hierarchy)
- How `Customer.allInstances` returns both Company and PrivatePerson objects
- How to create specific subtypes using `CreateNewObjectByExpressionName`
- How lookup/reference classes work with associations
- How derived attributes navigate associations (VATAmount = lineTotal * vatRate.percentage / 100)
- How the BoldGrid VAT Rate column uses `LookUpProperties` for dropdown selection

## The Extended Model

Step 5 extends the model from Steps 1-4 with inheritance and a new lookup class:

```
 ╔═══════════════════╗
 ║  Customer (base)  ║
 ╠═══════════════════╣
 ║ Name       String ║
 ║ Address    String ║
 ║ PostAddress String║
 ║ City       String ║        ╔═════════════════════╗
 ║ Country    String ╠═1══*═══╣      Invoice        ║
 ╚════════╤══════════╝        ╠═════════════════════╣
     ┌────┴────┐              ║ InvoiceNumber  Int  ║
     │         │              ║ InvoiceDate   Date  ║
 ╔═══╧═════╗ ╔╧════════════╗ ║ DueDate       Date  ║
 ║ Company ║ ║PrivatePerson║ ║ IsPaid     Boolean  ║
 ╠═════════╣ ╠═════════════╣ ║ /TotalAmt  Currency ║
 ║ VATNum  ║ ║ (no extra   ║ ╚══════════╤══════════╝
 ║ VATExem ║ ║  attributes)║        1   │   *
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

### What Changed from Steps 1-4

| Change | Before (Steps 1-4) | After (Step 5) |
|--------|-------------------|-----------------|
| Customer | Concrete class with Phone, Email | Abstract base with PostAddress, Country |
| Subtypes | None | Company (VATNumber, VATExempt), PrivatePerson |
| VATRate | Did not exist | New lookup class with Name, Percentage |
| InvoiceItem.VATRate | Did not exist | New 0..1 association to VATRate |
| InvoiceItem.VATAmount | Did not exist | New derived attribute: `lineTotal * vatRate.percentage / 100` |

## Concept 1: Inheritance in Bold

### What is Inheritance?

Inheritance models "is-a" relationships. A Company **is a** Customer. A PrivatePerson **is a** Customer. Both share the base Customer attributes (Name, Address, City, Country) but Company adds its own fields (VATNumber, VATExempt).

### How Bold Maps Inheritance to the Database

Bold uses **Table-Per-Hierarchy (TPH)**: all customer types share one database table. The table has columns for ALL attributes across the entire hierarchy, plus a `BOLD_TYPE` column that stores which concrete type each row represents.

```
CUSTOMER table
┌──────────┬──────┬─────────┬──────┬─────────┬───────────┬───────────┐
│BOLD_TYPE │ Name │ Address │ City │ Country │ VATNumber │ VATExempt │
├──────────┼──────┼─────────┼──────┼─────────┼───────────┼───────────┤
│ Company  │ Acme │ 123 St  │ NYC  │ US      │ US123456  │ False     │
│ PrivPrsn │ John │ 456 Ave │ LA   │ US      │ NULL      │ NULL      │
└──────────┴──────┴─────────┴──────┴─────────┴───────────┴───────────┘
```

**Advantages of TPH:**
- No joins needed to query all customers
- `Customer.allInstances` is a single SELECT
- Simple and fast

**Trade-off:**
- VATNumber and VATExempt columns are NULL for PrivatePerson rows (wasted space, but minimal)

### Polymorphic Queries with OCL

The power of inheritance shows in OCL queries:

```
Customer.allInstances                    → All customers (Company + PrivatePerson)
Company.allInstances                     → Only companies
PrivatePerson.allInstances               → Only private persons
Customer.allInstances->size              → Total customer count
Company.allInstances->size               → Company count only
```

These work because Bold knows the class hierarchy. `Customer.allInstances` returns every object whose type IS Customer or any subclass of Customer.

### Type Checking in OCL

```
customer.oclIsTypeOf(Company)            → True if this exact type is Company
customer.oclIsKindOf(Customer)           → True if this is any kind of Customer
customer.oclAsType(Company).vatNumber    → Cast to Company to access Company-specific attrs
```

### Creating Subtype Objects

Because Customer is abstract (in our model), you cannot create a Customer directly. You must create a specific subtype:

```pascal
// Create a Company
dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('Company');

// Create a PrivatePerson
dmStep5.BoldSystemHandle1.System.CreateNewObjectByExpressionName('PrivatePerson');
```

The new object automatically appears in:
- `Customer.allInstances` (because Company is-a Customer)
- `Company.allInstances` (because it IS a Company)

### Why Not Use the Navigator's Insert Button?

The TBoldNavigator Insert button creates objects based on the handle's expression type. For `Customer.allInstances`, it would try to create a `Customer` — but Customer is abstract. Instead, we use explicit buttons for each concrete type.

## Concept 2: Lookup Classes (VATRate)

### What is a Lookup Class?

A lookup class is a simple reference table. `VATRate` has just two attributes:
- **Name**: Human-readable label (e.g., "Standard 24%", "Reduced 14%", "Zero")
- **Percentage**: The actual rate as a number (e.g., 24.0, 14.0, 0.0)

Invoice items **point to** a VATRate via an association. This is the Bold equivalent of a foreign key to a lookup table in SQL.

### The InvoiceItem-VATRate Association

```
InvoiceItem  ──── 0..1 ────  VATRate
```

Each InvoiceItem can optionally have one VATRate (0..1). A VATRate can be used by many InvoiceItems. When you assign a VATRate to an item, the derived VATAmount recalculates automatically.

### VATAmount Derived Attribute

```
VATAmount = lineTotal * vatRate.percentage / 100
```

This OCL expression **navigates the association**: it reads `vatRate.percentage` from the linked VATRate object. When you:
- Change the quantity or unit price (which changes lineTotal)
- Or change which VATRate is linked
- Or change the VATRate's percentage

...the VATAmount automatically updates. This is Bold's reactive derivation in action.

### Dropdown Selection in BoldGrid

The Items grid shows a "VAT Rate" column that works as a dropdown selector. This is configured via the grid column's `LookUpProperties`:

```
Column.BoldProperties.Expression = 'vatRate'
Column.LookUpProperties.Expression = 'VATRate.allInstances'
```

The `LookUpProperties.Expression` tells Bold to show all VATRate instances as choices. The user picks one, and Bold sets the association automatically.

## Building Step 5

### Build Command

```powershell
C:\Attracs\DelphiStandards\DelphiBuildDPROJ.ps1 -Projectfile "examples\Tutorial\Step5_InheritanceVAT\Step5.dproj" -VerboseOutPut
```

Or open `Step5.dproj` in the Delphi IDE and press F9.

### Why Step 5 Has Its Own Data Module

Steps 1-4 share the model in `Shared/InvoiceDataModule.dfm`. Step 5 uses an extended model with new classes (Company, PrivatePerson, VATRate) and new associations. Since the Bold model is embedded in the DFM, Step 5 needs its own data module (`Step5DataModule`) with the extended model. The Pascal code is identical in structure.

### First Run: Setting Up Data

When you first run Step 5, the database is empty. Here is a suggested workflow:

1. **Create VAT rates first** (they are needed for invoice items):
   - Click in the VAT Rates grid, use the navigator to Insert
   - Add "Standard 24%" with Percentage 24.0
   - Add "Reduced 14%" with Percentage 14.0
   - Add "Zero" with Percentage 0.0

2. **Create customers**:
   - Click "New Company" — fill in Name, Address, City, Country, VATNumber
   - Click "New Person" — fill in Name, Address, City, Country
   - Notice the "Type" column shows "Company" or "PrivatePerson"

3. **Create invoices and items**:
   - Select a customer, use the Invoices navigator to Insert
   - Select the invoice, use the Items navigator to Insert
   - Fill in Description, Quantity, UnitPrice
   - Select a VAT Rate from the dropdown
   - Watch LineTotal and VATAmount calculate automatically

4. **Save** — click "Save Changes" to persist everything to SQLite

## Key Code Patterns

### Creating Objects by Type Name

```pascal
// Instead of: TCompany.Create(BoldSystem)
// Use: BoldSystem.CreateNewObjectByExpressionName('Company')
// This ensures Bold's internal tracking is set up correctly.
```

### Polymorphic Delete

```pascal
// lhaCustomers.CurrentBoldObject is typed as TCustomer
// but the actual object is TCompany or TPrivatePerson
// Delete works regardless of the concrete type:
lhaCustomers.CurrentBoldObject.Delete;
```

### Derived Attributes Navigating Associations

```
InvoiceItem.VATAmount = lineTotal * vatRate.percentage / 100
```

When `vatRate` is not assigned (nil), the expression evaluates to undefined, and the cell shows empty. Once you assign a VATRate, the amount appears instantly.

## Handle Chain

```
BoldSystemHandle1
     |
     +--> lhaCustomers       Expression='Customer.allInstances'
     |         |               (returns BOTH Company and PrivatePerson)
     |         |
     |         +--> lhaInvoices  Expression='invoices'
     |                   |
     |                   +--> lhaItems  Expression='items'
     |
     +--> lhaVATRates        Expression='VATRate.allInstances'
     |
     +--> lhaCompanies       Expression='Company.allInstances'
```

Note: `lhaCompanies` is available for future use (e.g., a Companies-only grid or a company count label). The current form uses `lhaCustomers` which shows all types.

## OCL Reference for Inheritance

| Expression | Returns |
|-----------|---------|
| `Customer.allInstances` | All customers (both types) |
| `Company.allInstances` | Only companies |
| `PrivatePerson.allInstances` | Only private persons |
| `oclType.name` | The type name as a string ("Company" or "PrivatePerson") |
| `oclIsTypeOf(Company)` | True if exact type is Company |
| `oclIsKindOf(Customer)` | True if type is Customer or any subclass |
| `oclAsType(Company).vatNumber` | Cast to Company, access VATNumber |
| `Company.allInstances->select(vatExempt)` | All VAT-exempt companies |

## Try It Yourself

### Exercise 1: Company-Only Invoice Total

Create a BoldLabel showing the total of all invoices for companies only:

```
Company.allInstances->collect(invoices)->collect(totalAmount)->sum.asString
```

### Exercise 2: VAT Summary

Create a BoldLabel showing total VAT across all items:

```
InvoiceItem.allInstances->collect(vatAmount)->sum.asString
```

### Exercise 3: Conditional Display

In a real application, you might want to show VATNumber only for companies. Try adding a column with:

```
if oclIsTypeOf(Company) then oclAsType(Company).vatNumber else '' endif
```

## Common Pitfalls

### Cannot Create Abstract Classes Directly

If Customer is marked abstract, `TBoldNavigator` Insert on `Customer.allInstances` will fail. Use explicit `CreateNewObjectByExpressionName` for each concrete type instead.

### Null Navigation in Derived Attributes

If an InvoiceItem has no VATRate assigned, `vatRate.percentage` navigates to null. The VATAmount will show as empty/undefined. This is correct behavior — the user should assign a VAT rate first.

### Database Schema Changes

Step 5 uses its own database file (`Step5.db`). If you previously ran Steps 1-4, those databases are separate and unaffected. If you need to reset Step 5, simply delete `Step5.db` and re-run.

## What's Next

You have completed all five tutorial steps! You now understand:

1. **Step 1**: Bold model structure and database persistence
2. **Step 2**: CRUD UIs with handles and grids
3. **Step 3**: Associations and master-detail relationships
4. **Step 4**: OCL queries for filtering and aggregation
5. **Step 5**: Inheritance hierarchies and lookup references

These concepts cover the core of Bold for Delphi. For advanced topics, explore:

- **Undo/Redo**: Bold's built-in undo mechanism (`TBoldUndoHandler`)
- **Constraints**: Model-level validation rules
- **Subscription system**: How Bold automatically refreshes UI when data changes
- **Database evolution**: Migrating schemas as your model evolves
