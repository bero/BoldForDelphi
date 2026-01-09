# Class Reference

This section documents the most important classes in Bold for Delphi.

## Class Hierarchy

```mermaid
classDiagram
    class TBoldElement {
        <<abstract>>
    }

    class TBoldDomainElement {
        <<abstract>>
    }

    class TBoldMember {
        <<abstract>>
        +BoldObject
        +OwningElement
    }

    class TBoldAttribute {
        +AsString
        +AsInteger
        +IsNull
    }

    class TBoldObjectReference {
        +BoldObject
    }

    class TBoldObjectList {
        +Count
        +Add()
        +Remove()
    }

    class TBoldObject {
        +BoldSystem
        +BoldMembers
        +Delete()
    }

    class TBoldSystem {
        +Classes
        +DirtyObjects
        +UpdateDatabase()
    }

    TBoldElement <|-- TBoldDomainElement
    TBoldDomainElement <|-- TBoldMember
    TBoldDomainElement <|-- TBoldObject
    TBoldMember <|-- TBoldAttribute
    TBoldMember <|-- TBoldObjectReference
    TBoldMember <|-- TBoldObjectList
    TBoldElement <|-- TBoldSystem
```

## Core Classes

| Class | Description | Documentation |
|-------|-------------|---------------|
| [TBoldSystem](TBoldSystem.md) | The Object Space - manages all objects | Core |
| [TBoldObject](TBoldObject.md) | Base class for all domain objects | Core |
| [TBoldObjectList](TBoldObjectList.md) | Collection of Bold objects | Core |
| [TBoldMember](TBoldMember.md) | Base class for attributes and references | Core |
| [TBoldAttribute](TBoldAttribute.md) | Stores attribute values | Core |

## Relationship Between Classes

```mermaid
flowchart TB
    System[TBoldSystem]
    ClassExtent[TBoldClassExtent]
    Object[TBoldObject]
    Attribute[TBoldAttribute]
    Reference[TBoldObjectReference]
    List[TBoldObjectList]

    System -->|"manages"| ClassExtent
    ClassExtent -->|"contains"| Object
    Object -->|"has members"| Attribute
    Object -->|"has members"| Reference
    Object -->|"has members"| List
    Reference -->|"points to"| Object
    List -->|"contains"| Object
```

## Common Patterns

### Accessing Objects

```pascal
// From system
var System: TBoldSystem := BoldSystemHandle1.System;

// Get all instances of a class
var Customers: TBoldObjectList := System.Classes['Customer'].BoldObjects;

// Navigate from object
var Orders: TBoldObjectList := Customer.Orders;
```

### Modifying Objects

```pascal
// Change attribute
Customer.Name := 'New Name';

// Add to list
Customer.Orders.Add(NewOrder);

// Delete object
Customer.Delete;
```

### Querying Objects

```pascal
// OCL query
var Result: TBoldObjectList := System.EvaluateExpressionAsNewElement(
  'Customer.allInstances->select(active)',
  nil
) as TBoldObjectList;
```
