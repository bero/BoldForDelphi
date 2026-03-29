# Step 4: OCL Expressions

In this final step, you will master **Object Constraint Language (OCL)**, Bold's query language. You will learn how to:

- Use OCL to filter collections (select)
- Transform and aggregate data (collect, sum, size)
- Write dynamic expressions in handles and labels
- Build real-time dashboards with BoldLabel

By the end, you will be able to write complex queries without touching SQL.

## What is OCL?

**Object Constraint Language** is a standard query language from the UML specification. It is designed to work with objects, not tables.

Instead of SQL:

```sql
SELECT SUM(lineTotal) FROM InvoiceItems WHERE invoiceId = 5
```

With OCL, you write:

```
invoices[invoiceNumber = 5].items.lineTotal->sum
```

Which reads naturally: "The sum of line totals from the items of the invoice with number 5."

OCL is:

- **Object-oriented**: Works with classes and references, not tables and joins
- **Expression-based**: Always evaluates to a value (doesn't execute side effects)
- **Type-safe**: Catches errors at parse time

## Why OCL Instead of SQL?

1. **No joins needed**: Relationships are properties. `invoice.customer.name` is simpler than a SQL join.
2. **Type-safe**: You get compile-time checking (in Bold's OCL compiler) instead of SQL runtime errors.
3. **Works in-memory**: OCL expressions work on objects in the BoldSystem, not just the database.
4. **Less code**: No parsing result sets. Objects are already objects.

## OCL Basics

### Navigation

Access an object's attributes and associations using dot notation:

```
customer.name                    → The name of the customer
invoice.customer.city            → The city of the invoice's customer
customer.invoices                → The list of invoices (a collection)
```

### Collections

Collections are groups of objects. You use the arrow operator `->` for collection operations:

```
customer.invoices->size          → How many invoices this customer has
customer.invoices->first         → The first invoice
customer.invoices->last          → The last invoice
```

### Select (Filter)

`select()` filters a collection based on a condition:

```
invoices->select(isPaid)         → All paid invoices
invoices->select(not isPaid)     → All unpaid invoices
invoices->select(totalAmount > 1000)  → Invoices over 1000
items->select(quantity > 5)      → Items with quantity > 5
```

Read it naturally: "Invoices where isPaid is true."

**In Code**:

```pascal
lhaUnpaidInvoices.Expression := 'invoices->select(not isPaid)';
```

Now the handle shows only unpaid invoices. As you mark invoices as paid, they disappear from the list automatically.

### Collect (Transform)

`collect()` extracts a specific attribute from each element in a collection:

```
invoices->collect(totalAmount)   → A list of the total amounts
items->collect(description)      → A list of item descriptions
```

**Practical use**: Get just the amounts for further processing:

```
invoices->collect(totalAmount)->sum  → The total of all invoice amounts
```

This works because:
1. `invoices` is a collection of Invoice objects
2. `->collect(totalAmount)` extracts the totalAmount from each (a collection of Currency)
3. `->sum` sums all the Currency values

### Sum (Aggregate)

`sum` adds up numeric values in a collection:

```
items->collect(lineTotal)->sum       → Total of all items in an invoice
invoices->collect(totalAmount)->sum  → Total of all invoices for a customer
```

Shorthand: Since lineTotal is a single attribute, you can skip collect:

```
items.lineTotal->sum  → Same as: items->collect(lineTotal)->sum
```

Here is how the derived attributes chain through the model:

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

 / = Derived. Bold evaluates the OCL expression automatically.
   When Quantity changes from 3→5, LineTotal updates to 250,
   which cascades: TotalAmount recalculates too. No code needed.
```

![Derived attributes in action](../images/step4-derived-attributes.png)
*Derived attributes cascade: changing Quantity recalculates LineTotal, which recalculates TotalAmount — all automatic*

### Size (Count)

`size` returns the number of elements:

```
invoices->size       → How many invoices this customer has
items->size          → How many items in this invoice
customers->size      → Total customers in the system
```

### Other Useful Operations

```
->isEmpty            → True if the collection is empty
->includes(obj)      → True if obj is in the collection
->notEmpty           → True if the collection has at least one element
->asString           → Convert to string (useful for display)
->sortedBy(attr)     → Sort by an attribute
```

Examples:

```
invoices->notEmpty                    → Does this customer have any invoices?
invoices->sortedBy(invoiceDate)       → Invoices in chronological order
items->select(quantity < 0).asString  → Items with negative quantity (data error)
```

## BoldLabel: Dynamic Read-Only Text

A **BoldLabel** displays a value that updates automatically. Unlike TBoldEdit, it is read-only.

### Use Cases

- Display totals
- Show counts
- Display derived fields
- Real-time dashboards

### How to Use

Drop a TBoldLabel on the form:

```
TBoldLabel
  Handle: lhaCustomers
  BoldAttribute: 'name'  // Shows the current customer's name
```

Now when you select a different customer, the label updates automatically.

### Dynamic Expressions with asString

For calculated values, use `asString` to convert to a string:

```
TBoldLabel
  Handle: lhaCustomers
  BoldAttribute: 'invoices->size.asString'  // Number of invoices
```

This shows a number like "5". As you add or delete invoices, the count updates in real-time.

## Building Step 4: The Dashboard

Let's build a form with these BoldLabels:

```
┌─────────────────────────────────────┐
│      Invoice Management             │
├─────────────────────────────────────┤
│ Total Customers: 15                 │
│ Selected Customer: Acme Corp         │
├─────────────────────────────────────┤
│ Customers Grid                      │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Name       | Address | City      │ │
│ └─────────────────────────────────┘ │
├─────────────────────────────────────┤
│ Invoices for Selected Customer: 7   │
│ Unpaid Invoices: 3                  │
│ Total Unpaid Amount: $4,567.89      │
├─────────────────────────────────────┤
│ Invoices Grid                       │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Number | Date | Total | IsPaid  │ │
│ └─────────────────────────────────┘ │
├─────────────────────────────────────┤
│ Items Grid                          │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Description | Quantity | Price   │ │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

![Step 4 dashboard with OCL labels](../images/step4-dashboard.png)
*The complete dashboard: BoldLabels show live counts and totals, the unpaid invoices grid filters automatically*

### Step 1: Add BoldLabels with Simple Expressions

```pascal
procedure TfrmStep4.CreateLabels;
begin
  // Total customer count
  lblTotalCustomers := TBoldLabel.Create(Self);
  lblTotalCustomers.Handle := lhaCustomers;
  lblTotalCustomers.BoldAttribute := 'Customer.allInstances->size.asString';
  lblTotalCustomers.Caption := 'Total Customers: ' + lblTotalCustomers.Text;

  // Selected customer name
  lblSelectedCustomer := TBoldLabel.Create(Self);
  lblSelectedCustomer.Handle := lhaCustomers;
  lblSelectedCustomer.BoldAttribute := 'name';
  lblSelectedCustomer.Caption := 'Selected: ' + lblSelectedCustomer.Text;
end;
```

Wait, that is not quite right. Let us try a cleaner approach using design-time dropped labels.

### Step 2: Design-Time Labels

1. Drop a TBoldLabel
   - Handle: lhaCustomers
   - BoldAttribute: 'Customer.allInstances->size.asString'
   - Set an initial Caption like "Total: 0"

2. Drop another TBoldLabel
   - Handle: lhaCustomers
   - BoldAttribute: 'invoices->size.asString'
   - Caption: "Invoices: 0"

3. Drop another for unpaid count
   - Handle: lhaCustomers
   - BoldAttribute: 'invoices->select(not isPaid)->size.asString'
   - Caption: "Unpaid: 0"

4. Drop another for total unpaid amount
   - Handle: lhaCustomers
   - BoldAttribute: 'invoices->select(not isPaid)->collect(totalAmount)->sum.asString'
   - Caption: "Total Unpaid: 0"

That last one is complex, but it reads clearly:
- Start with `invoices` (the customer's invoices)
- Filter with `->select(not isPaid)` (keep only unpaid)
- Extract amounts with `->collect(totalAmount)` (get the total of each)
- Sum with `->sum` (add them all)
- Convert to string with `.asString`

**Result**: A live-updating label showing the customer's total unpaid invoice amount.

## OCL Reference: Common Patterns

### Counting

```
items->size                          Number of items
invoices->select(isPaid)->size       Number of paid invoices
invoices->notEmpty.asString          "true" or "false"
```

### Aggregation

```
items.lineTotal->sum                 Total of all items
invoices.totalAmount->sum            Sum of all invoices
items->collect(quantity)->sum        Total quantity ordered
```

### Filtering

```
invoices->select(isPaid)             Only paid invoices
invoices->select(totalAmount > 1000) Only large invoices
items->select(quantity = 0)          Items with no quantity (data error)
```

### Sorting

```
invoices->sortedBy(invoiceDate)      Invoices in date order
items->sortedBy(description)         Items alphabetically
invoices->sortedBy(invoiceNumber)->reverse  Invoices in reverse order
```

### Checking Membership

```
invoices->includes(myInvoice)        True if myInvoice is in the list
invoices->first                      First invoice (or null if empty)
invoices->last                       Last invoice
```

### String Operations

```
customers.name->select(...)          Filter by name matching
items->sortedBy(description)         Sort alphabetically
```

## Try It Yourself

### Exercise 1: Create an Unpaid Invoices View

In the form, create a handle that shows only unpaid invoices:

```pascal
lhaUnpaidInvoices := TBoldListHandle.Create(nil);
lhaUnpaidInvoices.RootHandle := lhaCustomers;
lhaUnpaidInvoices.Expression := 'invoices->select(not isPaid)';

grdUnpaidInvoices.Handle := lhaUnpaidInvoices;
```

Now add a grid bound to this handle. As you mark invoices as paid, they disappear from the grid automatically. No code needed — the subscription system handles it.

### Exercise 2: Add a Filtered Items View

Show only items with quantity > 10:

```pascal
lhaLargeItems := TBoldListHandle.Create(nil);
lhaLargeItems.RootHandle := lhaItems;
lhaLargeItems.Expression := 'select(quantity > 10)';
```

### Exercise 3: Calculate Total Revenue

Create a BoldLabel showing the total of all invoices across all customers:

```
TBoldLabel
  Handle: lhaCustomers
  BoldAttribute: 'Customer.allInstances->collect(invoices)->collect(totalAmount)->sum.asString'
```

This is complex, but it demonstrates chaining multiple collection operations.

![Total revenue label](../images/step4-total-revenue.png)
*A single OCL expression calculates total revenue across all customers — no SQL, no loops, just objects*

Breakdown:
- `Customer.allInstances` - all customers
- `->collect(invoices)` - flatten all their invoices into one big collection
- `->collect(totalAmount)` - extract the total of each invoice
- `->sum` - add them all up

### Exercise 4: Dynamic Date Filtering

Many real-world queries filter by date. Try:

```
invoices->select(invoiceDate >= today - 30)->size.asString
```

Shows invoices from the last 30 days. (Note: `today` is an OCL constant.)

## Advanced OCL: forAll, exists

For more complex conditions:

```
items->forAll(quantity > 0)          True if all items have quantity > 0
items->exists(quantity = 0)          True if any item has quantity = 0
```

These are less common in UI expressions but useful for validation.

## Common Pitfalls

### OCL is Case-Sensitive

```
invoices->select(isPaid)             RIGHT (matches the attribute name)
invoices->select(is_paid)            WRONG (doesn't exist)
invoices->select(ISPAID)             WRONG (case matters)
```

Always match the exact case from your model.

### Collection Operations Must Come After a Collection

This is wrong:

```
customer.name->select(length > 3)    ERROR: name is a String, not a collection
```

Name is a single value. You can only use `->` on collections. This would be:

```
customer.invoices->select(totalAmount > 1000)  RIGHT: invoices is a collection
```

### Arrow vs. Dot

- **Dot (.)** for accessing attributes: `customer.name`
- **Arrow (->)** for collection operations: `invoices->size`

Remember: Arrow for collections (the `-` points to "multiple").

### Null Handling

If the current element is null, `customer.invoices` also returns null (empty). OCL does not throw errors on null. It just propagates null gracefully.

```
if CurrentElement is null:
  invoices->size returns 0
  invoices->select(...) returns empty collection
```

### Performance: Complex Expressions

Very complex expressions can be slow if they involve many objects. For dashboards, this is usually fine. For high-frequency updates, consider caching or using stored calculated attributes.

## What's Next

You have completed the full tutorial! You now understand:

1. **Step 1**: How Bold models structure with UML and persists to databases
2. **Step 2**: How to build CRUD UIs with handles and grids
3. **Step 3**: How associations model relationships and enable master-detail
4. **Step 4**: How OCL queries replace SQL

## Further Learning

- **MasterDetail Example**: Open `examples/Simple/ObjectSpace/MasterDetail/` for a real-world master-detail application
- **Bold Wiki**: https://delphi.fandom.com/wiki/Bold_for_Delphi — comprehensive documentation
- **Bold Blog**: http://boldfordelphi.blogspot.com/ — tips and tricks
- **Object Constraint Language Standard**: https://www.omg.org/spec/OCL/ — formal specification

## Final Exercises

### 1. Build a Invoice Report

Create a form that displays:

```
Customer: [name]
Total Invoices: [invoices->size]
Paid: [invoices->select(isPaid)->size]
Unpaid: [invoices->select(not isPaid)->size]
Total Unpaid: [invoices->select(not isPaid)->collect(totalAmount)->sum]
```

All fields update live as you modify data.

### 2. Implement Data Validation

Add a BoldLabel showing errors:

```
Data Issues:
- [items->select(quantity < 0)->size] items with negative quantity
- [items->select(unitPrice < 0)->size] items with negative price
```

Users can immediately see data quality issues.

### 3. Create a Dashboard

Combine multiple grids and labels:

```
┌──────────────────────────────┐
│ Active Customers: [count]    │
│ Total Revenue: [sum]         │
│ Overdue Invoices: [count]    │
├──────────────────────────────┤
│ [Customer Grid]              │
│ [Invoice Grid]               │
│ [Items Grid]                 │
└──────────────────────────────┘
```

Each section updates independently.

## Conclusion

Bold for Delphi is a powerful framework that eliminates boilerplate code. By understanding models, handles, associations, and OCL, you can build sophisticated data applications quickly and correctly.

The key insight: **Design objects, not tables. The rest follows automatically.**

---

**Key Takeaways**:
- OCL is a query language designed for objects, not tables
- `->select()` filters collections
- `->collect()` transforms and extracts
- `->sum`, `->size` aggregate data
- `->sortedBy()` orders results
- BoldLabel with `.asString` creates live-updating read-only text
- Complex expressions are built by chaining operations
- No SQL, no result set parsing, just objects and collections
