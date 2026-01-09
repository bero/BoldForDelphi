# Frequently Asked Questions

## Getting Started

### How do I create a new object?

```pascal
// Method 1: Using typed constructor
var Customer := TCustomer.Create(BoldSystem);

// Method 2: Using class extent
var Customer := BoldSystem.Classes['Customer'].CreateNewObject as TCustomer;
```

### How do I save changes to the database?

```pascal
BoldSystemHandle1.UpdateDatabase;
// or
BoldSystem.UpdateDatabase;
```

### How do I delete an object?

```pascal
Customer.Delete;
BoldSystem.UpdateDatabase;  // Actually removes from DB
```

### How do I check for unsaved changes?

```pascal
if BoldSystem.DirtyObjects.Count > 0 then
  ShowMessage('You have unsaved changes');
```

---

## OCL Queries

### How do I get all instances of a class?

```pascal
// Method 1: OCL
var Customers := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances', nil) as TBoldObjectList;

// Method 2: Direct access
var Customers := BoldSystem.Classes['Customer'].BoldObjects;
```

### How do I filter objects?

```pascal
// Find active customers
var ActiveCustomers := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->select(active = true)', nil) as TBoldObjectList;

// Find customers with orders
var CustomersWithOrders := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->select(orders->notEmpty)', nil) as TBoldObjectList;
```

### How do I sort a list?

```pascal
// Sort by name
var Sorted := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->orderBy(name)', nil) as TBoldObjectList;

// Sort descending
var Sorted := BoldSystem.EvaluateExpressionAsNewElement(
  'Customer.allInstances->orderDescending(createdDate)', nil) as TBoldObjectList;
```

### How do I calculate a sum?

```pascal
var Total := BoldSystem.EvaluateExpressionAsFloat(
  'Order.allInstances->collect(total)->sum', nil);
```

---

## Associations

### How do I link two objects?

```pascal
// One-to-many: Add order to customer
Customer.Orders.Add(Order);

// Or set the reverse
Order.Customer := Customer;

// Many-to-many: Add to either side
Student.Courses.Add(Course);
// or
Course.Students.Add(Student);
```

### How do I unlink objects?

```pascal
// Remove from list
Customer.Orders.Remove(Order);

// Or set reference to nil
Order.Customer := nil;
```

### How do I navigate associations?

```pascal
// One-to-many
for Order in Customer.Orders do
  ProcessOrder(Order);

// Get parent
var Customer := Order.Customer;

// Chain navigation
for Line in Customer.Orders[0].OrderLines do
  ProcessLine(Line);
```

---

## Null Values

### How do I check if an attribute is null?

```pascal
if Customer.M_Email.IsNull then
  ShowMessage('No email address');
```

### How do I set an attribute to null?

```pascal
Customer.M_Email.SetToNull;
```

### How do I handle nulls safely?

```pascal
var Email: string;
if Customer.M_Email.IsNull then
  Email := 'N/A'
else
  Email := Customer.Email;
```

---

## Transactions

### How do I use transactions?

```pascal
BoldSystem.StartTransaction;
try
  // Make changes
  Customer.Name := 'New Name';
  Order.Delete;

  BoldSystem.CommitTransaction;
  BoldSystem.UpdateDatabase;
except
  BoldSystem.RollbackTransaction;
  raise;
end;
```

### What happens if I don't use transactions?

Changes are still tracked but won't be atomic. Multiple changes could partially save if an error occurs during `UpdateDatabase`.

---

## UI Binding

### How do I bind a grid to a list?

```pascal
// Set up list handle
BoldListHandle1.RootHandle := BoldSystemHandle1;
BoldListHandle1.Expression := 'Customer.allInstances';

// Bind grid
BoldGrid1.BoldHandle := BoldListHandle1;
```

### How do I bind an edit to an attribute?

```pascal
BoldEdit1.BoldHandle := CustomerHandle;
BoldEdit1.BoldProperties.Expression := 'name';
```

### Why isn't my UI updating?

1. Check that handles are connected properly
2. Verify the expression is correct
3. Ensure object is part of the system
4. Check that subscriptions are not blocked

---

## Performance

### How do I prefetch related objects?

```pascal
// Fetch customers and their orders in fewer queries
BoldSystem.FetchLinksWithObjects(CustomerList, 'orders');
```

### Why are my queries slow?

1. Use batch fetching instead of lazy loading
2. Add database indexes on frequently queried columns
3. Use OCL `->select()` to filter in database, not in memory
4. Avoid `allInstances` on large tables without filtering

### How do I reduce memory usage?

```pascal
// Unload objects not currently needed
BoldSystem.Classes['LargeClass'].UnloadAll;
```

---

## Troubleshooting

### "Object not found" error

The object may have been deleted or not yet saved. Check:
```pascal
if (Obj <> nil) and (Obj.BoldExistenceState = esExisting) then
  // Safe to use
```

### "Cannot modify read-only object"

The object may be from a derived list or unloaded. Ensure you have the actual object from the system.

### Database update fails

1. Check for constraint violations
2. Verify database connection is active
3. Look for objects with invalid foreign keys
4. Check for circular dependencies

### OCL expression error

1. Verify class and attribute names match the model
2. Check for typos in navigation paths
3. Ensure types are compatible in comparisons
4. Use parentheses to clarify complex expressions

---

## Migration

### How do I update the database schema?

Use Bold's DbEvolutor to generate migration scripts:

```pascal
BoldDbEvolutor1.GenerateScript;
// Review and execute the generated SQL
```

### Can I use Bold with an existing database?

Yes, but you need to:
1. Create a UML model matching the existing schema
2. Configure table/column mappings in tagged values
3. Import existing data carefully

---

## Resources

- [GitHub Repository](https://github.com/ArtDabbler/BoldForDelphi)
- [Bold Wiki](https://delphi.fandom.com/wiki/Bold_for_Delphi)
- [Discord Community](https://discord.gg/C6frzsn)
