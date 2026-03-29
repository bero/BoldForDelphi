# Step 2: Customer CRUD

In this step, you will build your first Bold UI. You will learn how to:

- Use **TBoldListHandle** to view objects in the BoldSystem
- Use **TBoldGrid** to display them in a table
- Use **TBoldNavigator** to create and delete objects
- Use **TBoldEdit** to view and edit a single attribute
- Understand how Bold components stay synchronized automatically

By the end, you will have a working customer management interface with zero SQL and minimal code.

## What You Will Learn

- What is a BoldHandle and why do we need it?
- How to use OCL expressions to select which objects to display
- How TBoldGrid and other UI components bind to objects
- How to save changes back to the database
- How to handle dirty objects (unsaved changes)

## What is a BoldHandle?

A **Handle** is a "window" into the BoldSystem. It evaluates an OCL expression to select a subset of objects:

```
BoldSystem (contains all objects)
    |
    +-- TBoldListHandle (with expression: Customer.allInstances)
    |
    +-- Shows: All Customer objects currently in memory
```

Without a handle, you would need to write code like:

```pascal
var Customers: TBoldObjectList;
begin
  Customers := BoldSystem.AllInstances(TCustomer);
  for I := 0 to Customers.Count - 1 do
    DisplayCustomer(Customers[I]);
end;
```

With a handle, Bold does all this for you:

```pascal
var Handle: TBoldListHandle;
begin
  Handle.Expression := 'Customer.allInstances';
  // Bold automatically selects the customers, tracks the current one,
  // and notifies UI components when the list changes
end;
```

Handles remove boilerplate code. They are the bridge between the object-oriented BoldSystem and the traditional data-binding world of Delphi UI components.

## Key Concepts: TBoldListHandle

### What It Does

```
TBoldListHandle
  RootHandle: BoldSystemHandle1           // Where to start (the BoldSystem)
  Expression: 'Customer.allInstances'    // What to select
  CurrentElement                          // The selected object (like a cursor)
```

When you set the `Expression`:

1. Bold evaluates it against the BoldSystem
2. The handle maintains a list of matching objects
3. The handle tracks a "current element" (the selected row)
4. All UI components connected to the handle stay synchronized

### CurrentElement (Like a Database Cursor)

A handle acts like a database cursor. It points to one "current" object:

- When you click a row in the grid, the handle's `CurrentElement` changes
- All other UI components (TBoldEdit, TBoldLabel) connected to the handle update to show the current object's data

**Example**: If you have:
- `lhaCustomers` with `CurrentElement = Customer #1`
- `edtName` (TBoldEdit) connected to `lhaCustomers` and attribute Name

Then `edtName` automatically displays Customer #1's Name. If you click a different row, `edtName` updates to that customer's name. No code required!

## Building the Customer CRUD UI

Let's build a form with these components:

```
┌─────────────────────────────────────┐
│         Customer Management         │
├─────────────────────────────────────┤
│ [Insert] [Delete] [Refresh]         │
├─────────────────────────────────────┤
│ Grid showing all customers          │
│ (Name, Address, City, Phone, Email) │
├─────────────────────────────────────┤
│ Name:  [______________]             │
│ City:  [______________]             │
│ Email: [______________]             │
│                                     │
│ [Save] [Undo]                       │
└─────────────────────────────────────┘
```

![Step 2 Customer CRUD form](../images/step2-form.png)
*The completed Customer CRUD form with grid, navigator, edit fields, and Save/Undo buttons*

### Step 1: Add TBoldListHandle

1. Open `Step2Form.pas` (or create it)
2. Add a `TBoldListHandle` component
   - Name it `lhaCustomers`
   - Set `RootHandle` to `BoldSystemHandle1` (from the data module)
   - Set `Expression` to `Customer.allInstances`

This tells Bold: "Show me all Customer objects in the BoldSystem."

### Step 2: Add TBoldGrid

1. Add a `TBoldGrid` component from the Bold tab
   - Name it `grdCustomers`
   - Set `Handle` to `lhaCustomers`
   - Set `Align` to `alClient`

Run the application. The grid should show an empty table with columns: Name, Address, City, Phone, Email. (Empty because no customers exist yet.)

### Step 3: Add TBoldNavigator

1. Add a `TBoldNavigator` component
   - Name it `navCustomers`
   - Set `Handle` to `lhaCustomers`

The navigator has buttons:
- **Insert**: Creates a new Customer and adds it to the grid
- **Delete**: Removes the selected customer from the grid
- **Refresh**: Reloads the list (useful if the database changed externally)

### Step 4: Add TBoldEdit Fields

Add these TBoldEdit components to display/edit the selected customer:

```
TBoldEdit (Name)
  Handle: lhaCustomers
  BoldAttribute: 'Name'     // Which attribute to display

TBoldEdit (City)
  Handle: lhaCustomers
  BoldAttribute: 'City'

TBoldEdit (Email)
  Handle: lhaCustomers
  BoldAttribute: 'Email'
```

Now when you select a customer in the grid, the TBoldEdit fields update to show that customer's data. Edit any field and it updates the object in memory.

### Step 5: Add Save and Undo Buttons

1. Add a "Save" button
   - In OnClick:
   ```pascal
   dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
   ```

2. Add an "Undo" button
   - In OnClick:
   ```pascal
   dmInvoice.BoldSystemHandle1.System.RollBack;
   ```

## How Bold Components Work

When you drop a TBoldEdit on a form and set its Handle and BoldAttribute:

1. **Initialization**: The component asks the handle for the current object
2. **Display**: It reads the attribute from that object and shows it
3. **Edit**: When you type, it writes to the object in the BoldSystem (not the database)
4. **Sync**: When the handle's `CurrentElement` changes (you click a different row), all connected components update

This is **data-binding without data-binding code**. There is no OnChange event wiring, no manual updates. Bold handles it automatically through its subscription system.

### The Subscription System

Behind the scenes, Bold uses a **subscription pattern**:

- The TBoldEdit subscribes to notifications from the object it is bound to
- When the object's attribute changes, the object notifies all subscribers
- The TBoldEdit gets the notification and updates its visual display

This means:

```pascal
Customer.Name := 'New Name';       // Code changes the object
edtName.Text;                       // TBoldEdit automatically knows and updates
ShowMessage(edtName.Text);          // Shows 'New Name'
```

No event handling needed!

## Understanding OCL Expressions

We used the expression `Customer.allInstances`. What does that mean?

- **Customer**: The class (like a table name in SQL)
- **.allInstances**: An OCL operation that means "all instances of this class"

This is equivalent to SQL: `SELECT * FROM Customers`

But instead of SQL, you use **Object Constraint Language (OCL)**, which is part of the UML standard. OCL works with objects, not tables:

```
Customer.allInstances       →   All Customer objects
orders->size                →   The number of orders this customer has
invoices->select(not isPaid) →   This customer's unpaid invoices
```

More on OCL in Step 4.

## Try It Yourself

### Exercise 1: Add Customers Through the UI

1. Run the application
2. Click the "Insert" button in the navigator
3. A new row appears in the grid with default values
4. Edit the Name field: type "Acme Corp"
5. Click on another row (or another field)
6. The new customer is in memory
7. Click "Save"
8. The customer is now in the database

Now restart the application. The customer is still there! Bold loaded it from the database.

![Step 2 with sample customers](../images/step2-grid-with-data.png)
*The grid populated with customer data — all persisted to SQLite and loaded automatically on startup*

### Exercise 2: Use the Grid to Edit

You can edit directly in the grid:

1. Click a cell in the grid (Name column)
2. Type a new value
3. The TBoldEdit fields at the bottom update automatically
4. Click Save

### Exercise 3: Delete a Customer

1. Select a customer (click a row)
2. Click the Delete button
3. The row disappears
4. Click Save

The customer is deleted from the database.

### Exercise 4: Undo Changes

1. Edit a customer's name
2. Do NOT click Save
3. Click the Undo button
4. The grid reverts to the saved state

Undo discards all unsaved changes in the BoldSystem.

### Exercise 5: Try an Invalid Expression

Change the handle expression to something invalid, like:

```
InvalidClass.allInstances
```

Bold will show an error. Expressions are checked at runtime. This is useful for dynamic queries, but you should always validate expressions before use in production.

## The Form's Close Query

The form has this code:

```pascal
procedure TfrmStep2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if dmInvoice.BoldSystemHandle1.System.HasDirtyObjects then
  begin
    if MessageDlg('You have unsaved changes. Save before closing?',
      mtWarning, [mbYes, mbNo], 0) = mrYes then
      dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
  end;
  CanClose := True;
end;
```

![Dirty objects prompt](../images/step2-dirty-prompt.png)
*Bold detects unsaved changes and prompts before closing — no manual tracking needed*

This checks for unsaved changes before closing:

- **HasDirtyObjects**: Returns true if any object in the BoldSystem has been modified
- If there are dirty objects, prompt the user to save
- If they click Yes, save to the database
- If they click No, discard changes (Undo is implicit on shutdown)

This is a common pattern in database applications: prompt before losing data.

## Common Pitfalls

### Trying to Bind to Private Attributes

TBoldEdit requires attributes to be public. You cannot bind to private fields.

If you add an attribute to the model, make sure it is not marked as private (or derived). The code generator creates public properties by default, so this is usually not an issue.

### Forgetting to Call UpdateDatabase

Changes in memory do not go to the database automatically:

```pascal
Customer.Name := 'New Name';
// At this point, the BoldSystem has the change, but the database does not
// You must call:
dmInvoice.BoldSystemHandle1.System.UpdateDatabase;
```

If you forget, the changes are lost when the application closes.

### Modifying the List While Iterating

If you delete objects from `lhaCustomers.List` while a for loop is iterating, the loop can miss items. Always iterate over a copy, or use a while loop with careful indexing:

```pascal
// Right: Iterate backwards
for I := lhaCustomers.List.Count - 1 downto 0 do
  lhaCustomers.List[I].Delete;

// Wrong: Iterate forwards while deleting
for I := 0 to lhaCustomers.List.Count - 1 do
  lhaCustomers.List[I].Delete;  // Can skip items!
```

## Next Step

You now have a working customer UI. But customers without invoices are boring!

Proceed to [`Step3_Invoices/README.md`](../Step3_Invoices/README.md) to learn how to add invoices and master-detail views.

---

**Key Takeaways**:
- TBoldListHandle selects objects via OCL expressions
- TBoldGrid displays a list of objects; click to change CurrentElement
- TBoldEdit binds to a single attribute of the current object
- All components stay synchronized automatically via subscriptions
- Changes are in-memory until you call UpdateDatabase
- Check HasDirtyObjects before closing to avoid losing data
