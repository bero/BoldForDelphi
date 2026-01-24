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

    click TBoldElement href "TBoldElement/" "TBoldElement documentation"
    click TBoldSystem href "TBoldSystem/" "TBoldSystem documentation"
    click TBoldObject href "TBoldObject/" "TBoldObject documentation"
    click TBoldObjectList href "TBoldObjectList/" "TBoldObjectList documentation"
    click TBoldMember href "TBoldMember/" "TBoldMember documentation"
    click TBoldAttribute href "TBoldAttribute/" "TBoldAttribute documentation"
    click TBoldObjectReference href "TBoldObjectReference/" "TBoldObjectReference documentation"
```

## Core Classes

| Class | Description |
|-------|-------------|
| [TBoldElement](TBoldElement.md) | Abstract base class for all Bold elements |
| [TBoldSystem](TBoldSystem.md) | The Object Space - manages all objects |
| [TBoldObject](TBoldObject.md) | Base class for all domain objects |
| [TBoldObjectList](TBoldObjectList.md) | Collection of Bold objects |
| [TBoldMember](TBoldMember.md) | Base class for attributes and references |
| [TBoldAttribute](TBoldAttribute.md) | Stores attribute values |
| [TBoldObjectReference](TBoldObjectReference.md) | Single-valued associations |

## Handle Classes

| Class | Description |
|-------|-------------|
| [TBoldSystemHandle](TBoldSystemHandle.md) | Manages the Object Space connection |
| [TBoldListHandle](TBoldListHandle.md) | OCL list expressions with filtering and sorting |
| [TBoldExpressionHandle](TBoldExpressionHandle.md) | Single value OCL expressions |

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

    click System href "TBoldSystem/" "TBoldSystem documentation"
    click Object href "TBoldObject/" "TBoldObject documentation"
    click Attribute href "TBoldAttribute/" "TBoldAttribute documentation"
    click Reference href "TBoldObjectReference/" "TBoldObjectReference documentation"
    click List href "TBoldObjectList/" "TBoldObjectList documentation"
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
