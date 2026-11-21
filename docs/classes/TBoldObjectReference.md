# TBoldObjectReference

`TBoldObjectReference` (unit `BoldSystem`) is the member for a single-valued association end. It refers to one other object or to nil.

## Class Definition

Public members, abridged:

```pascal
TBoldObjectReference = class(TBoldMember)
public
  // The referenced object
  property BoldObject: TBoldObject;        // fetches the object when needed
  property Locator: TBoldObjectLocator;    // does not fetch the object
  property IsEmpty: Boolean;               // True when no object is referenced
  procedure Clear;

  // Checks before a change
  function CanSet(NewObject: TBoldObject; Subscriber: TBoldSubscriber): Boolean;
  function CanClear(Subscriber: TBoldSubscriber): Boolean;

  // Metadata and state
  property BoldRoleRTInfo: TBoldRoleRTInfo;
  property HasOldValues: Boolean;
end;
```

## Class Hierarchy

```mermaid
classDiagram
    TBoldMember <|-- TBoldObjectReference
    TBoldMember <|-- TBoldAttribute
    TBoldMember <|-- TBoldList
    TBoldList <|-- TBoldObjectList

    TBoldObjectReference : +BoldObject
    TBoldObjectReference : +HasOldValues

    click TBoldMember href "../TBoldMember/" "TBoldMember documentation"
    click TBoldAttribute href "../TBoldAttribute/" "TBoldAttribute documentation"
    click TBoldObjectList href "../TBoldObjectList/" "TBoldObjectList documentation"
```

## Understanding References

In UML, associations between classes can be:

| Multiplicity | Bold Class | Example |
|--------------|------------|---------|
| 0..1 or 1 | TBoldObjectReference | Order.customer |
| 0..* or 1..* | TBoldObjectList | Customer.orders |

```mermaid
erDiagram
    Customer ||--o{ Order : "has"
    Order }o--|| Customer : "belongs to"
```

## Properties

### BoldObject

The referenced object (or nil):

```pascal
var
  Order: TOrder;
  Customer: TCustomer;
begin
  Order := GetCurrentOrder;

  // Get the referenced customer
  Customer := Order.M_customer.BoldObject as TCustomer;

  // Or use the generated property
  Customer := Order.customer;
end;
```

## Working with References

### Reading a Reference

```pascal
var
  Order: TOrder;
  Customer: TCustomer;
begin
  Order := GetCurrentOrder;

  // Check if reference is set
  if Assigned(Order.customer) then
    ShowMessage('Customer: ' + Order.customer.Name)
  else
    ShowMessage('No customer assigned');
end;
```

### Setting a Reference

```pascal
var
  Order: TOrder;
  Customer: TCustomer;
begin
  Order := GetCurrentOrder;
  Customer := GetSelectedCustomer;

  // Set the reference
  Order.customer := Customer;

  // Bold automatically maintains the inverse relationship
  // Customer.orders now includes this Order
end;
```

### Clearing a Reference

```pascal
// Set to nil
Order.customer := nil;

// Or using the raw member
Order.M_customer.BoldObject := nil;
Order.M_customer.Clear;
```

## Bidirectional Associations

Bold automatically maintains both ends of a bidirectional association:

```pascal
// Setting Order.customer automatically adds Order to Customer.orders
Order.customer := Customer;

// These are now true:
Assert(Order.customer = Customer);
Assert(Customer.orders.Includes(Order));
```

```mermaid
flowchart LR
    subgraph Before
        O1[Order]
        C1[Customer]
    end

    subgraph After["After: Order.customer := Customer"]
        O2[Order] -->|customer| C2[Customer]
        C2 -->|orders| O2
    end
```

## Raw Member Access

Use `M_` prefix for low-level access:

```pascal
var
  Ref: TBoldObjectReference;
begin
  Ref := Order.M_customer;

  // Check for an empty reference without fetching the referenced object
  if Ref.IsEmpty then
    ShowMessage('No customer');

  // Subscribe to changes
  Ref.DefaultSubscribe(Subscriber);
end;
```

## Comparing with Attributes

| Aspect | TBoldAttribute | TBoldObjectReference |
|--------|----------------|----------------------|
| Stores | Simple values | Object pointer |
| Example | Name: String | customer: TCustomer |
| Null check | IsNull | IsEmpty, or BoldObject = nil |
| Generated property | Customer.Name | Order.customer |

## Common Patterns

### Safe Navigation

```pascal
function GetCustomerName(Order: TOrder): string;
begin
  if Assigned(Order.customer) then
    Result := Order.customer.Name
  else
    Result := '(no customer)';
end;
```

### Assign with Validation

```pascal
procedure AssignCustomer(Order: TOrder; Customer: TCustomer);
begin
  if not Assigned(Customer) then
    raise Exception.Create('Customer is required');

  if not Customer.Active then
    raise Exception.Create('Cannot assign inactive customer');

  Order.customer := Customer;
end;
```

### OCL Navigation

```pascal
// Navigate reference in OCL
BoldListHandle1.Expression := 'Order.allInstances->select(customer.active)';

// Get all customers who have orders
BoldListHandle2.Expression := 'Order.allInstances->collect(customer)->asSet';
```

## Subscription

Subscribe to reference changes. `OrderCustomerSubscriber` is a `TBoldPassthroughSubscriber` field of the form:

```pascal
procedure TForm1.SubscribeToCustomerChange(Order: TOrder);
begin
  if not Assigned(OrderCustomerSubscriber) then
    OrderCustomerSubscriber := TBoldPassthroughSubscriber.Create(OrderCustomerSubscriberReceive);
  Order.M_customer.DefaultSubscribe(OrderCustomerSubscriber);
end;

procedure TForm1.OrderCustomerSubscriberReceive(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  // Reference changed
  UpdateCustomerDisplay;
end;
```

## See Also

- [TBoldObjectReference reference](../reference/BoldSystem/TBoldObjectReference.md) - Generated API reference
- [TBoldMember](TBoldMember.md) - Base class
- [TBoldObjectList](TBoldObjectList.md) - Multi-valued associations
- [TBoldObject](TBoldObject.md) - Domain objects
- [Subscriptions](../concepts/subscriptions.md) - Change notification
