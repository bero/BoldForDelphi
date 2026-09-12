# Constraints

A UML constraint is a boolean OCL expression attached to a class or an
attribute. Bold evaluates constraints continuously, exposes each one as a
`TBAConstraint` member you can navigate to in OCL, and lets you refuse a save
while any of them is false.

This demo puts an ordering model behind three different uses of that: marking
offending cells, listing every broken constraint in the system, and blocking
`UpdateDatabase`.

## Running it

There is no `.dproj`. Open `BE_ConstraintsDemo.dpr` in the IDE and let Delphi
create one.

It needs a database. Persistence is InterBase through
`TBoldPersistenceHandleDB` / `TBoldDatabaseAdapterIB` / `TIBDatabase`, with
`sysdba` / `masterkey` in the form, and **`IBDatabase1.DatabaseName` is empty**,
so set it first. Then **Create DB**, then **Open system**, then add a customer,
an order and some order items and watch the bottom grid fill up.

`fConstraintsMain.dfm` is a binary form file that the repository's
`*.dfm text eol=crlf` rule inflates on checkout (20254 bytes against 20148 in
the object store), so it will not stream until you restore it with
`git show HEAD:<path> > <path>`.

## The constraints in the model

They live as tagged values on the classes, so nothing in the Delphi code
mentions them.

| Element | Constraint |
| --- | --- |
| `Customer` | `(creditCardNo = '') implies (creditLimit < 1000)` |
| `Customer` | `shippingAddress <> ''` |
| `Customer` | `name <> ''` |
| `Customer.name` | `self <> ''` |
| `Order` | `orderSum <= customer.creditLimit` |
| `Order` | `orderDate < shippingDate` |
| `OrderItem` | `qty > 0` |
| `OrderItem` | `part->notEmpty` |
| `OrderItem` | `itemSum > 100` |
| `Part` | `price > 0` |

Two of them lean on derived attributes: `OrderItem.itemSum` is
`qty * part.price` and `Order.orderSum` is `items.itemsum->sum`. So changing a
part's price can break a constraint on an order two navigations away, and the
constraint notices.

## The three uses

**In the grids.** `BoldGrid1` to `BoldGrid4` set `BoldShowConstraints = True`,
which is all it takes for Bold to mark what is wrong.

**As a list.** `blhBrokenConstraints` is an ordinary `TBoldListHandle` with

```
BusinessClassesRoot.allinstances.constraints->select(c|not c)
```

`constraints` is a real member on every object, so this collects every
constraint of every object in the system and keeps the false ones. It updates
itself as you type.

`BoldGrid5` renders that list through `TBAConstraint`'s string representations
rather than through expressions:

| Column | Representation | What it returns |
| --- | --- | --- |
| Name | 13 | the owning element's `DisplayName` |
| Message | 11 | the constraint's message |
| Expr | 12 | the constraint's OCL text |
| ElementType | 13 | the owning element's `DisplayName` again |
| Offending Element | 14 | the owning element `AsString` |

Columns 1 and 4 carry the same representation, so they show the same text.
Representation 10, the constraint's own model name, is the one that is missing.

Double-clicking a row jumps to the object at fault:

```pascal
AutoFormProviderRegistry.FormForElement(
  (blhBrokenConstraints.CurrentElement as TBAConstraint).OwningElement).Show;
```

**At save time.** `ConstraintExampleClasses.inc` overrides `MayUpdate` on the
model root:

```pascal
result := EvaluateExpressionAsString('constraints->select(a|not a)->size = 0', brDefault) = 'Y';
```

and on failure builds a `TBoldFailureReason` from
`constraints->select(a|not a)->first` read in representation 10, which is the
broken constraint's model name.

`MayUpdate` alone would only be asked for created and deleted objects, so
`ReceiveQueryFromOwned` catches `bqMayUpdate` coming from an owned member and
routes it to `MayUpdate` as well. That second override is what subjects a plain
edit of an existing object to the check.

## Files

| File | What it holds |
| --- | --- |
| `BE_ConstraintsDemo.dpr` | creates `TForm1` and runs |
| `fConstraintsMain.pas` / `.dfm` | handles, grids and the embedded model |
| `ConstraintExampleClasses.inc` | `MayUpdate` and `ReceiveQueryFromOwned` |
| `ConstraintExampleClasses*.pas/.inc` | generated business classes |
