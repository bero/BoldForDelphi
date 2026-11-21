# Step 4: OCL Expressions

In this step you learn the **Object Constraint Language (OCL)**, Bold's query language. You will:

- Filter collections (`select`)
- Transform and aggregate them (`collect`, `sum`, `size`)
- Write expressions in handles and labels
- Build a dashboard that updates by itself with `TBoldLabel`

By the end, you can write complex queries without SQL.

## What is OCL?

The **Object Constraint Language** is a query language from the UML standard. It works on objects, not tables.

In SQL you would write:

```sql
SELECT SUM(i.Quantity * i.UnitPrice)
FROM InvoiceItem i JOIN Invoice v ON ...
WHERE v.InvoiceNumber = 5
```

In OCL you write:

```
Invoice.allInstances->select(invoiceNumber = 5).items.lineTotal->sum
```

It reads: "the sum of the line totals of the items of the invoice with number 5".

OCL is:

- **Object-oriented**: it works with classes and references, not tables and joins.
- **Expression-based**: it always evaluates to a value and changes nothing.
- **Type-checked**: Bold checks the names and types when it parses an expression.

## Why OCL Instead of SQL?

1. **No joins**: relationships are members. `invoice.customer.name` is simpler than a join.
2. **Checked early**: Bold reports a misspelled member when it parses the expression, with its position.
3. **Works in memory**: OCL works on the objects in the object space, including changes not saved yet.
4. **Less code**: no result sets to read; objects stay objects.

## OCL Basics

### Navigation

Reach an object's attributes and associations with a dot:

```
customer.name                    → the name of the customer
invoice.customer.city            → the city of the invoice's customer
customer.invoices                → the invoices of the customer (a collection)
```

### Collections

A collection is a group of objects or values. Collection operations take an arrow, `->`:

```
customer.invoices->size          → how many invoices this customer has
customer.invoices->first         → the first invoice
customer.invoices->last          → the last invoice
```

### Select (Filter)

`select()` keeps the elements that satisfy a condition:

```
invoices->select(isPaid)               → all paid invoices
invoices->select(not isPaid)           → all unpaid invoices
invoices->select(totalAmount > 1000)   → invoices over 1000
items->select(quantity > 5)            → items with a quantity over 5
```

Read it as: "the invoices where isPaid is true".

**In code**:

```pascal
lhaInvoices.Expression := 'invoices->select(not isPaid)';
```

The handle now holds only unpaid invoices. When you mark an invoice as paid, it leaves the list.

### Collect (Transform)

`collect()` takes a value from each element of a collection:

```
invoices->collect(totalAmount)   → a list of the invoice totals
items->collect(description)      → a list of the item descriptions
```

You can work on the result further:

```
invoices->collect(totalAmount)->sum  → the total of all invoice amounts
```

Step by step:

1. `invoices` is a collection of Invoice objects.
2. `->collect(totalAmount)` takes the totalAmount of each, a collection of Currency values.
3. `->sum` adds the values up.

### Sum (Aggregate)

`sum` adds up the numbers of a collection:

```
items->collect(lineTotal)->sum       → the total of the items of an invoice
invoices->collect(totalAmount)->sum  → the total of the invoices of a customer
```

A shorthand: navigating with a dot from a collection collects too, so you can leave `collect` out:

```
items.lineTotal->sum                 → the same as items->collect(lineTotal)->sum
```

This is how the derived attributes chain through the model:

```
 ╔═══════════════╗       ╔═══════════════╗       ╔═══════════════╗
 ║   Customer    ║       ║    Invoice    ║       ║  InvoiceItem  ║
 ╠═══════════════╣       ╠═══════════════╣       ╠═══════════════╣
 ║               ║──1:*─►║               ║──1:*─►║ Quantity    3 ║
 ║               ║       ║ /TotalAmount  ║       ║ UnitPrice  50 ║
 ║               ║       ║       ▲       ║       ║ /LineTotal 150║
 ╚═══════════════╝       ╚═══════╬═══════╝       ╚═══════╬═══════╝
                                 │                        │
                    items.lineTotal->sum        quantity * unitPrice
                         = 150 + ...                  = 3 * 50

 / = derived. Bold evaluates the OCL expression.
   When Quantity changes from 3 to 5, LineTotal becomes 250,
   and TotalAmount follows. No code is needed.
```

### Size (Count)

`size` gives the number of elements:

```
invoices->size                  → how many invoices this customer has
items->size                     → how many items this invoice has
Customer.allInstances->size     → how many customers there are
```

### Other Useful Operations

```
->isEmpty                  → true if the collection is empty
->notEmpty                 → true if the collection has at least one element
->includes(obj)            → true if obj is in the collection
->orderby(attr)            → the collection sorted by an attribute
->orderdescending(attr)    → sorted, largest first
.asString                  → the value as a string, for display
```

Examples:

```
invoices->notEmpty                       → does this customer have invoices?
invoices->orderby(invoiceDate)           → invoices in date order
items->select(quantity < 0)->size        → items with a negative quantity (a data error)
```

## TBoldLabel: Read-Only Text That Updates Itself

A **TBoldLabel** shows the value of an OCL expression and updates when the value changes. Unlike `TBoldEdit`, it is
read-only.

### Use Cases

- Totals
- Counts
- Derived values
- Dashboards

### How to Use It

Drop a `TBoldLabel` (page **Bold Controls**) on the form and set:

```
TBoldLabel
  BoldHandle: lhaCustomers
  BoldProperties.Expression: 'name'    // the current customer's name
```

When you select another customer, the label shows its name.

### Text and Numbers in One Expression

The label shows the result of the expression, so put the text in the expression too. `+` joins strings, and
`asString` turns a number into a string:

```
TBoldLabel
  BoldHandle: lhaCustomers
  BoldProperties.Expression: ('Invoices: ' + invoices->size.asString)
```

The label shows "Invoices: 5", and the number follows as you add or delete invoices. OCL strings use single quotes;
in Delphi code you double them, as in any Delphi string.

## The Step 4 Form: a Dashboard

The finished form (`Step4Form.pas`) looks like this:

```
┌─────────────────────────────────────────────────────────────────┐
│ Step 4: OCL Expressions                     Total Customers: 15 │
├─────────────────────────────────────────────────────────────────┤
│ All Unpaid Invoices (OCL filter)                                │
│ Customer | Invoice # | Due Date | Amount                        │
├──────────────────────────┬──────────────────────────────────────┤
│ Customers                │ Invoices for Customer (7)            │
│ Name       | City        │ Invoice # | Date | Paid | Total      │
│                          │ [|<] [<] [>] [>|] [+] [-]            │
│                          │ Invoice Line Items                   │
│ Outstanding: 4567.89     │ Description | Qty | Unit Price | ... │
│ [|<] [<] [>] [>|] [+] [-]│ [|<] [<] [>] [>|] [+] [-]            │
├──────────────────────────┴──────────────────────────────────────┤
│ [Save Changes] [Open/Close system]   status                     │
└─────────────────────────────────────────────────────────────────┘
```

It has the handle chain of Step 3 (`lhaCustomers`, `lhaInvoices`, `lhaItems`), one more list handle, and three
labels.

### The Labels

All three labels have `BoldHandle` = `lhaCustomers`:

| Label | `BoldProperties.Expression` |
|-------|-----------------------------|
| `lblCustomerCount` | `('Total Customers: ' + Customer.allInstances->size.asString)` |
| `lblInvoiceCount` | `('(' + invoices->size.asString + ')')` |
| `lblOutstanding` | `('Outstanding: ' + invoices->select(not isPaid)->collect(totalAmount)->sum.asString)` |

`Customer.allInstances` starts from a class, so it gives the same result whichever customer is current. The other
two start from the current customer.

The last expression reads:

- Start with `invoices`, the current customer's invoices.
- Keep the unpaid ones: `->select(not isPaid)`.
- Take the amount of each: `->collect(totalAmount)`.
- Add them up: `->sum`.
- Turn the result into a string: `.asString`.

**Result**: a label with the customer's unpaid amount, which updates as you edit invoices and items.

### The Unpaid Invoices Grid

The fourth list handle, `lhaUnpaidInvoices`, is not part of the chain:

```
lhaUnpaidInvoices: TBoldListHandle
  RootHandle: BoldSystemHandle1
  Expression: 'Invoice.allInstances->select(not isPaid)'
```

The grid `grdUnpaid` shows it, with the columns `customer.name`, `invoiceNumber`, `dueDate` and `totalAmount`. A
column expression can navigate too: `customer.name` shows the name of each invoice's customer. Mark an invoice as
paid, and it leaves this grid.

## OCL Reference: Common Patterns

### Counting

```
items->size                          number of items
invoices->select(isPaid)->size       number of paid invoices
invoices->notEmpty.asString          'True' or 'False'
```

### Aggregation

```
items.lineTotal->sum                 total of all items
invoices.totalAmount->sum            sum of all invoices
items->collect(quantity)->sum        total quantity ordered
```

### Filtering

```
invoices->select(isPaid)             only paid invoices
invoices->reject(isPaid)             only unpaid invoices
invoices->select(totalAmount > 1000) only large invoices
items->select(quantity = 0)          items without a quantity (a data error)
```

### Sorting

```
invoices->orderby(invoiceDate)           invoices in date order
items->orderby(description)              items alphabetically
invoices->orderdescending(invoiceNumber) invoices, highest number first
```

### Membership and Position

```
invoices->includes(myInvoice)        true if myInvoice is in the list
invoices->first                      the first invoice (nil if there is none)
invoices->last                       the last invoice
```

### Strings

```
name.toUpper                                     the name in capitals
name.length                                      the length of the name
Customer.allInstances->select(name.sqlLike('A%'))  customers whose name starts with A
```

## Try It Yourself

### Exercise 1: An Unpaid Invoices View per Customer

The form's unpaid grid shows the unpaid invoices of all customers. Add a handle for those of the current customer
only:

```pascal
lhaUnpaidOfCustomer.RootHandle := lhaCustomers;
lhaUnpaidOfCustomer.Expression := 'invoices->select(not isPaid)';
grdUnpaidOfCustomer.BoldHandle := lhaUnpaidOfCustomer;
```

Mark an invoice as paid, and it leaves the grid. The subscriptions do it; you write no code.

### Exercise 2: A Filtered Items View

Show only the items of the current invoice with a quantity over 10:

```pascal
lhaLargeItems.RootHandle := lhaInvoices;
lhaLargeItems.Expression := 'items->select(quantity > 10)';
```

### Exercise 3: Total Revenue

Add a `TBoldLabel` with the total of all invoices of all customers:

```
TBoldLabel
  BoldHandle: lhaCustomers
  BoldProperties.Expression: Customer.allInstances->collect(invoices)->collect(totalAmount)->sum.asString
```

Step by step:

- `Customer.allInstances`: all customers
- `->collect(invoices)`: all their invoices, in one collection
- `->collect(totalAmount)`: the total of each invoice
- `->sum`: added up

### Exercise 4: Overdue Invoices

Many real queries filter on dates. `today` is a variable that OCL always knows: today's date. Count the unpaid
invoices of the current customer whose due date has passed:

```
invoices->select((not isPaid) and (dueDate < today))->size.asString
```

## Advanced OCL: forAll and exists

For conditions on a whole collection:

```
items->forAll(quantity > 0)          true if every item has a quantity over 0
items->exists(quantity = 0)          true if any item has a quantity of 0
```

These are less common in UI expressions but useful for validation.

## Common Pitfalls

### Names in OCL

OCL uses the expression names of the model, which differ from the Delphi names in one letter:

- A member name starts with a lowercase letter: the attribute `IsPaid` is `isPaid` in OCL.
- A class name starts with an uppercase letter: `Customer.allInstances`.

At the start of an expression, and alone inside `select(...)`, a name that starts with an uppercase letter is read
as a class name, so `select(IsPaid)` is an error.

```
invoices->select(isPaid)             RIGHT
invoices->select(IsPaid)             WRONG: read as a class name
invoices->select(is_paid)            WRONG: no such member
```

### Collection Operations Need a Collection

This is wrong:

```
customer.name->select(length > 3)    ERROR: name is a String, not a collection
```

`name` is a single value, and `select` works on collections only. This is right:

```
customer.invoices->select(totalAmount > 1000)   RIGHT: invoices is a collection
```

### Arrow and Dot

- The **dot** (`.`) for attributes and associations: `customer.name`
- The **arrow** (`->`) for collection operations: `invoices->size`

### No Current Element

When a handle has no current element, for example when the customer list is empty, the labels and grids that
depend on it have nothing to show.

### Performance of Complex Expressions

An expression over many objects can be slow, because Bold loads the objects it navigates. For a dashboard this is
usually fine. When a value is needed very often, consider a derived attribute or a stored value instead.

## What's Next

You have learned:

1. **Step 1**: how a Bold model defines the structure and how Bold persists objects
2. **Step 2**: how to build a CRUD UI with handles and grids
3. **Step 3**: how associations model relationships and give master-detail views
4. **Step 4**: how OCL queries replace SQL

In Step 5 the model grows with inheritance and a lookup class. Go to
[`Step5_InheritanceVAT/README.md`](../Step5_InheritanceVAT/README.md).

## Further Learning

- **MasterDetail example**: `examples/Simple/ObjectSpace/MasterDetail/`, another master-detail application
- **OCL Queries**: https://bero.github.io/BoldForDelphi/concepts/ocl/
- **Bold Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi
- **Bold Blog**: http://boldfordelphi.blogspot.com/
- **The OCL standard**: https://www.omg.org/spec/OCL/

## More Exercises

### 1. An Invoice Report

Build a form that shows, for the current customer:

```
Customer:     name
Invoices:     invoices->size
Paid:         invoices->select(isPaid)->size
Unpaid:       invoices->select(not isPaid)->size
Total Unpaid: invoices->select(not isPaid)->collect(totalAmount)->sum
```

Each line is a `TBoldLabel`; add `.asString` to the numbers. All values update as you change data.

### 2. Data Validation

Add labels that show data problems:

```
items->select(quantity < 0)->size     items with a negative quantity
items->select(unitPrice < 0)->size    items with a negative price
```

Users see data quality issues at once.

### 3. A Dashboard

Combine grids and labels:

```
┌──────────────────────────────┐
│ Customers:        [count]    │
│ Total Revenue:    [sum]      │
│ Overdue Invoices: [count]    │
├──────────────────────────────┤
│ [Customer grid]              │
│ [Invoice grid]               │
│ [Items grid]                 │
└──────────────────────────────┘
```

Each part updates by itself.

---

**Key Takeaways**:

- OCL is a query language for objects, not tables
- `->select()` filters a collection
- `->collect()` takes a value from each element
- `->sum` and `->size` aggregate
- `->orderby()` sorts
- A `TBoldLabel` shows an OCL expression and updates by itself; `asString` and `+` build its text
- Complex expressions are chains of simple operations
- No SQL, no result sets: objects and collections
