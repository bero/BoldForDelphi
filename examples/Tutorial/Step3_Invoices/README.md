# Step 3: Invoices and Associations

In this step, you will learn how Bold models relationships between objects. You will build a master-detail UI: select a customer to see their invoices, select an invoice to see its items.

This is where Bold really shines. Without Bold, you would write SQL joins and complex queries. With Bold, you use **handle chaining**: a simple pattern where handles reference other handles to build hierarchical views.

## What You Will Learn

- What is an association in Bold?
- How do associations work? (One side, many side, bidirectional references)
- What is handle chaining and how does it enable master-detail views?
- How to create related objects (e.g., add an invoice to a customer)
- What are derived attributes and how does Bold calculate them?

## Associations: Modeling Relationships

Recall the model from Step 1. The associations are the connections between classes:

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

The `═══` lines are **associations**. Each has two sides you can navigate:

**Forward (one → many)** — returns a list:

```pascal
MyCustomer.Invoices          // all invoices for this customer
MyCustomer.Invoices[0]       // first invoice
MyCustomer.Invoices.Count    // how many invoices
MyInvoice.Items              // all line items for this invoice
```

**Backward (many → one)** — returns a single object:

```pascal
MyInvoice.Customer           // the customer this invoice belongs to
MyInvoice.Customer.Name      // that customer's name
MyItem.Invoice               // the invoice this item belongs to
MyItem.Invoice.InvoiceNumber // that invoice's number
```

**Bidirectional**: Bold maintains both sides automatically. Add an invoice to a customer's list, and the invoice's `.Customer` is set too. Delete an invoice, and it disappears from the customer's `.Invoices` list.

Let's look deeper at what this means in code.

### The Customer Side

From the Customer class, you can access the list of invoices:

```pascal
var Customer: TCustomer;
begin
  Customer := GetSomeCustomer;
  ShowMessage('This customer has ' + IntToStr(Customer.Invoices.Count) + ' invoices');

  // Iterate the invoices
  for I := 0 to Customer.Invoices.Count - 1 do
    ShowMessage(Customer.Invoices[I].InvoiceNumber.ToString);
end;
```

The `Customer.Invoices` property is a list (TInvoiceList). Bold auto-created this from the association.

### The Invoice Side

From the Invoice class, you can reference back to the customer:

```pascal
var Invoice: TInvoice;
begin
  Invoice := GetSomeInvoice;
  ShowMessage('This invoice belongs to: ' + Invoice.Customer.Name);
end;
```

The `Invoice.Customer` property is a single reference. Bold auto-created this too.

### Bidirectional Sync

This is the key insight: **Bold keeps both sides in sync automatically**. If you do this:

```pascal
Invoice.Customer := Customer;  // Set the invoice's customer
```

Then automatically:

```pascal
Customer.Invoices.Includes(Invoice)  // The invoice appears in the customer's list
```

You don't need to call `Customer.Invoices.Add(Invoice)`. Bold does it for you. Set one side, and Bold sets the other.

Conversely, if you do:

```pascal
Customer.Invoices.Add(Invoice);     // Add an invoice to the customer
```

Then automatically:

```pascal
Invoice.Customer = Customer         // The invoice's customer is set
```

This bidirectional sync is automatic and atomic. It is one of Bold's most powerful features.

## Handle Chaining: Master-Detail Made Simple

Now that you understand associations, let's use them to build a master-detail UI.

### The Concept

Instead of one TBoldListHandle showing all customers, create a chain:

```
┌──────────────────────────────────┐
│ BoldSystemHandle1 (root)         │
│ Contains all objects in memory   │
└──────────┬───────────────────────┘
           │
┌──────────▼──────────────────────┐
│ lhaCustomers                     │
│ Expression: Customer.allInstances│
│ CurrentElement: Customer #5      │
└──────────┬───────────────────────┘
           │ (scope)
┌──────────▼──────────────────────┐
│ lhaInvoices                      │
│ RootHandle: lhaCustomers         │
│ Expression: invoices             │ ← Invoices of the current customer
│ CurrentElement: Invoice #23      │
└──────────┬───────────────────────┘
           │ (scope)
┌──────────▼──────────────────────┐
│ lhaItems                         │
│ RootHandle: lhaInvoices          │
│ Expression: items                │ ← Items of the current invoice
│ CurrentElement: Item #1          │
└──────────────────────────────────┘
```

![Handle chain visualization](../images/step3-handle-chain.png)
*The three-level handle chain in action: selecting a customer filters invoices, selecting an invoice filters items*

When you select a customer (change `lhaCustomers.CurrentElement`), all downstream handles automatically re-evaluate:

- `lhaInvoices` shows that customer's invoices
- `lhaItems` shows that invoice's items

This happens automatically! No code needed.

### Implementing Handle Chaining

In the form:

```pascal
procedure TfrmStep3.CreateHandles;
begin
  // Root handle: all customers
  lhaCustomers := TBoldListHandle.Create(nil);
  lhaCustomers.RootHandle := dmInvoice.BoldSystemHandle1;
  lhaCustomers.Expression := 'Customer.allInstances';

  // Chained handle: invoices of selected customer
  lhaInvoices := TBoldListHandle.Create(nil);
  lhaInvoices.RootHandle := lhaCustomers;  // ← Key: root is the customer handle, not the system
  lhaInvoices.Expression := 'invoices';    // ← Invoices of the current customer

  // Nested handle: items of selected invoice
  lhaItems := TBoldListHandle.Create(nil);
  lhaItems.RootHandle := lhaInvoices;      // ← Root is the invoice handle
  lhaItems.Expression := 'items';          // ← Items of the current invoice

  // Connect UI components
  grdCustomers.Handle := lhaCustomers;
  grdInvoices.Handle := lhaInvoices;
  grdItems.Handle := lhaItems;
end;
```

**Key insight**: When you set `RootHandle` to another handle (not the BoldSystemHandle), Bold evaluates the expression in the scope of that handle's current element.

- `lhaInvoices.Expression = 'invoices'` means: "The invoices collection of the current customer"
- If you click Customer #5, lhaInvoices now shows Customer #5's invoices
- If you click Customer #3, lhaInvoices shows Customer #3's invoices

All automatic. No code in grid OnClick handlers.

## Creating Related Objects

When you add an invoice through the UI:

1. Click the Insert button on the `grdInvoices` navigator
2. Bold creates a new Invoice and adds it to the currently selected customer's invoices list
3. The bidirectional sync sets the invoice's customer reference
4. The new invoice appears in the grid

This works because the invoice grid is bound to `lhaInvoices`, which is rooted on `lhaCustomers.invoices`. When you insert, Bold knows to create the object in that context.

## Derived Attributes (Read-Only Calculated Fields)

In Step 1, we planned to create two derived attributes:

- **Invoice.TotalAmount**: The sum of all item line totals
- **InvoiceItem.LineTotal**: Quantity times UnitPrice

Derived attributes are read-only fields that Bold calculates automatically using OCL expressions.

### Adding Derived Attributes to the Model

In the Model Editor:

1. Select the InvoiceItem class
2. Right-click and "New Attribute"
3. Name: `LineTotal`
4. Type: Currency
5. Mark as **Derived**
6. Set the derivation formula: `quantity * unitPrice`

Do the same for Invoice:

1. Select the Invoice class
2. Right-click and "New Attribute"
3. Name: `TotalAmount`
4. Type: Currency
5. Mark as **Derived**
6. Set the derivation formula: `items.lineTotal->sum`

The formula `items.lineTotal->sum` means:
- Start with the current invoice's items
- Extract the lineTotal from each item
- Sum all the values

### How Derived Attributes Work

When you read a derived attribute:

```pascal
var Total: Currency;
begin
  Total := Invoice.TotalAmount;  // Bold evaluates the OCL expression
end;
```

Bold:
1. Evaluates `items.lineTotal->sum` in the context of that invoice
2. Calculates the result
3. Returns the currency value

If you change a line item's quantity, the invoice's `TotalAmount` automatically reflects the new sum. Bold's subscription system notifies all observers of the change.

### Displaying Derived Attributes

In the grid, derived attributes appear as read-only columns:

```
Invoice Grid Columns:
  InvoiceNumber (editable)
  InvoiceDate (editable)
  IsPaid (editable)
  TotalAmount (read-only, calculated)  ← Derived
```

Users can see the totals update in real-time as they edit quantities.

## Building the Step 3 Form

The form has three grids arranged vertically:

```
┌─────────────────────────────────────┐
│     Customers                       │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Name       | Address | City | .. │
│ ├─────────────────────────────────┤ │
│ │ Acme Corp  | 123 Main | Boston   │
│ │ Widget Inc | 456 Oak  | Seattle  │
│ └─────────────────────────────────┘ │
├─────────────────────────────────────┤
│     Invoices                        │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Number | Date | DueDate | Total  │
│ ├─────────────────────────────────┤ │
│ │ 001    | ... | ...      | 1500.00│
│ │ 002    | ... | ...      | 2300.00│
│ └─────────────────────────────────┘ │
├─────────────────────────────────────┤
│     Invoice Items                   │
│ [Insert] [Delete]                   │
│ ┌─────────────────────────────────┐ │
│ │ Description | Quantity | Price.. │
│ ├─────────────────────────────────┤ │
│ │ Widgets     | 100      | 15.00   │
│ │ Shipping    | 1        | 0.00    │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
```

![Step 3 master-detail form](../images/step3-master-detail.png)
*Three-panel master-detail layout: Customers, Invoices for selected customer, Items for selected invoice*

### Steps to Build It

1. **Create three TBoldListHandle components**:
   ```
   lhaCustomers (RootHandle: BoldSystemHandle1, Expression: Customer.allInstances)
   lhaInvoices (RootHandle: lhaCustomers, Expression: invoices)
   lhaItems (RootHandle: lhaInvoices, Expression: items)
   ```

2. **Create three TBoldGrid components** and wire them:
   ```
   grdCustomers.Handle := lhaCustomers
   grdInvoices.Handle := lhaInvoices
   grdItems.Handle := lhaItems
   ```

3. **Create three TBoldNavigator components**:
   ```
   navCustomers.Handle := lhaCustomers
   navInvoices.Handle := lhaInvoices
   navItems.Handle := lhaItems
   ```

4. **Configure the grids** to show the right columns. In the grid designer, add columns for each attribute you want to display.

5. **Add a Save button** that calls `dmInvoice.BoldSystemHandle1.System.UpdateDatabase`.

### The Magic: No Master-Detail Code Required

In traditional database programming, you would:

1. Load customers from the database
2. Attach a OnClick handler to the customer grid
3. In the handler, query the database for that customer's invoices
4. Refresh the invoice grid
5. Attach a OnClick handler to the invoice grid
6. In that handler, query for the invoice's items
7. Refresh the item grid

With Bold:

1. Create three handles
2. Chain them together
3. Done. Click and everything updates automatically.

This is the power of a properly designed ORM: object relationships become UI relationships automatically.

## Try It Yourself

### Exercise 1: Add Some Data

1. Run the application
2. Click "Insert" in the Customers navigator
3. Type a name (e.g., "Acme Corp")
4. Press Tab to move to the next field
5. Click on the Invoices grid
6. Click "Insert" in the Invoices navigator
7. A new invoice appears with default values
8. Edit the invoice number (e.g., "001")
9. Click on the Items grid
10. Click "Insert" in the Items navigator
11. Edit the item description and quantity
12. Watch the invoice's TotalAmount update automatically!
13. Click Save

Now you have a complete invoice with line items.

![Step 3 with invoice data](../images/step3-with-data.png)
*A customer with invoices and line items — TotalAmount updates automatically as you add items*

### Exercise 2: Test Bidirectional Sync

1. Select a customer (click their row in the grid)
2. Select an invoice from that customer
3. Change the invoice's customer by selecting a different customer in the first grid and attempting to reassign
4. Watch the invoice move to the new customer's list

(This requires code to reassign, but the concept is: change the customer reference and Bold updates both sides.)

### Exercise 3: Delete an Invoice and Watch Totals Update

1. Select an invoice
2. Select one of its line items
3. Click Delete in the Items navigator
4. The item is removed
5. The invoice's TotalAmount updates automatically
6. Click Save

### Exercise 4: Add Another Invoice Type

Try creating a Quotation class similar to Invoice, but with different attributes. Wire up the handles and grids. This exercises your understanding of handle chaining.

## Filtering with Handle Expressions

You can use more complex OCL expressions in handles:

```
Paid invoices only:     invoices->select(isPaid)
Unpaid totaling > 1000: invoices->select(not isPaid and totalAmount > 1000)
Recent invoices:        invoices->select(invoiceDate > today - 30)
```

Try changing the expression in the invoice handle to:

```
lhaInvoices.Expression := 'invoices->select(not isPaid)';
```

Now only unpaid invoices appear. As you mark invoices as paid, they disappear from the grid automatically.

## Common Mistakes

### Wrong Root Handle

If you set:

```
lhaInvoices.RootHandle := dmInvoice.BoldSystemHandle1;  // WRONG
lhaInvoices.Expression := 'invoices';
```

This tries to evaluate `invoices` on the BoldSystem, which doesn't have an invoices property. You get an error.

Always chain to the appropriate parent handle:

```
lhaInvoices.RootHandle := lhaCustomers;  // RIGHT
```

### Forgetting Bidirectional Sync

When you delete an invoice, Bold automatically removes it from the customer's invoices list. You don't need to write code. Trust the association.

### Editing Derived Attributes

Derived attributes are read-only. This form tries to edit them and fails:

```pascal
Invoice.TotalAmount := 5000;  // ERROR: TotalAmount is derived, read-only
```

Let Bold calculate them. Edit only the base attributes (Quantity, UnitPrice).

## Next Step

You now have a working invoice application with associations and master-detail views!

In the final step, you will learn OCL expressions in depth and build dynamic queries and labels.

Proceed to [`Step4_OclExpressions/README.md`](../Step4_OclExpressions/README.md).

---

**Key Takeaways**:
- Associations model relationships between classes (1:1, 1:N)
- Bold keeps both sides of an association in sync automatically
- Handle chaining enables master-detail views without code
- Derived attributes are OCL expressions that Bold calculates automatically
- Click to select, and all downstream handles re-evaluate automatically
- No SQL joins, no manual refreshes, no event handling needed
